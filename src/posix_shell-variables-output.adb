------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2015, AdaCore                   --
--                                                                          --
-- GSH is free software;  you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GSH is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GSH is maintained by AdaCore (http://www.adacore.com)                    --
--                                                                          --
------------------------------------------------------------------------------

with Posix_Shell.Subst; use Posix_Shell.Subst;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with OS.Exec;
with Ada.Strings.Unbounded;
pragma Warnings (Off);
with System.CRTL;
pragma Warnings (On);
with GNAT.Task_Lock;

package body Posix_Shell.Variables.Output is

   type Pipe_Type is record
      Input, Output : GNAT.OS_Lib.File_Descriptor;
   end record;

   function Create_Pipe (Pipe : not null access Pipe_Type) return Integer;
   pragma Import (C, Create_Pipe, "__gnat_pipe");

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor;

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup, "__gnat_dup");

   procedure Close (S : Shell_State; N : Integer) is
   begin
      Close (Get_Fd (S, N));
   end Close;

   ----------------
   -- Close_Pipe --
   ----------------

   procedure Close_Pipe (S : in out Shell_State) is
   begin
      Close (S, 0);
      --  restore stdin
      S.Redirections (0) := S.Redirections (-2);
   end Close_Pipe;

   function Get_Fd
     (S : Shell_State; N : Integer) return File_Descriptor
   is
   begin
      return S.Redirections (N).Fd;
   end Get_Fd;

   -----------
   -- Error --
   -----------

   procedure Error (S : Shell_State; Msg : String) is
   begin
      Put (S, 2, "[error] " & Msg & ASCII.LF);
   end Error;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (S : Shell_State; IO : Integer) is
   begin
      Put (S, IO, String'(1 => ASCII.LF));
   end New_Line;

   -----------------------
   -- Push_Redirections --
   -----------------------

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Append
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Open_Append, "__gnat_open_append");
   begin
      return C_Open_Append (Name, Fmode);
   end Open_Append;

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Open_Append (C_Name (C_Name'First)'Address, Fmode);
   end Open_Append;

   ----------------------
   -- Pop_Redirections --
   ----------------------

   procedure Restore_Descriptors
     (State       : in out Shell_State;
      Descriptors : Shell_Descriptors;
      In_Place    : Boolean := False)
   is
   begin
      if In_Place then
         return;
      end if;

      declare
         Previous : constant Shell_Descriptors := State.Redirections;
         Success  : Boolean;
      begin
         GNAT.Task_Lock.Lock;
         State.Redirections := Descriptors;

         for I in 0 .. Previous'Last loop
            --  Close the current redirection file descriptor only if it was
            --  not the same as the one we want to restore and if it is not
            --  stdin, stdout or stderr
            if Previous (I).Can_Be_Closed then
               Close (Previous (I).Fd);
               if Previous (I).Delete_On_Close then
                  Delete_File (Previous (I).Filename.all, Success);
               end if;
            end if;
         end loop;
         GNAT.Task_Lock.Unlock;
      end;
   end Restore_Descriptors;

   -----------------------
   -- Push_Redirections --
   -----------------------

   function Apply_Redirections
     (State        : in out Shell_State;
      Redirections : Redirection_Stack;
      In_Place     : Boolean := False)
     return Boolean
   is

      Has_Command_Subst : Boolean;

      Success    : Boolean := False;
      Old_States : constant Shell_Descriptors := State.Redirections;
      New_States : Shell_Descriptors;
      On_Windows : constant Boolean := Directory_Separator = '\';
      Has_Errors : Boolean := False;
      --  Will be set to true if some redirection were not put in place

      function Is_Null_File (Str : String) return Boolean;
      pragma Inline (Is_Null_File);
      --  return True if the filename correspond the name of the null file
      --  (for example "dev/null" on Unix systems).

      function Resolve_Filename (A : Token) return String;

      procedure Open_FD
         (FD              : Natural;
          Path            : String;
          Write           : Boolean := True;
          Append          : Boolean := False;
          Delete_On_Close : Boolean := False);
      --  @ Open a file descriptor
      --  @
      --  @ :param Natural FD: File descriptor that needs to be created
      --  @ :param String Path: Path to the file that should be opened
      --  @ :param Boolean Write: If True (default) the file is open for
      --  @     writing otherwise it is opened for reading.
      --  @ :param Boolean Append: If True then when a file is open in write
      --  @     mode content will be appended to the previous content.
      --  @     Otherwise previous content is discarded. In read mode the
      --  @     flag has no effect.
      --  @ :param Boolean Delete_On_Close: If True the file will be deleted
      --  @     when the file descriptor is closed

      -------------
      -- Open_FD --
      -------------

      procedure Open_FD
         (FD              : Natural;
          Path            : String;
          Write           : Boolean := True;
          Append          : Boolean := False;
          Delete_On_Close : Boolean := False)
      is
      begin
         New_States (FD).Filename := new String'(Path);

         if Write and then not Append and then
            not Is_Null_File (Path)
         then
            Delete_File (Path, Success);
         end if;

         GNAT.Task_Lock.Lock;
         if Write then
            New_States (FD).Fd := Open_Append (Path, Binary);
         else
            New_States (FD).Fd := Open_Read (Path, Binary);
         end if;

         OS.Exec.Set_Close_On_Exec (New_States (FD).Fd, True, Success);
         New_States (FD).Delete_On_Close := Delete_On_Close;
         New_States (FD).Can_Be_Closed := True;
         if Append and then Write then
            Lseek (New_States (FD).Fd, 0, Seek_End);
         end if;
         GNAT.Task_Lock.Unlock;
      end Open_FD;

      ------------------
      -- Is_Null_File --
      ------------------

      function Is_Null_File (Str : String) return Boolean is
      begin

         if On_Windows and then Str = "NUL" then
            return True;
         end if;

         if not On_Windows and then Str = "/dev/null" then
            return True;
         end if;

         return False;

      end Is_Null_File;

      function Resolve_Filename (A : Token) return String
      is
         Eval_Result : constant String := Resolve_Path
           (State,
            Eval_String_Unsplit (State, Get_Token_String (A),
              Has_Command_Subst => Has_Command_Subst));
      begin
         if On_Windows and then Eval_Result = "/dev/null" then
            return "NUL";
         else
            return Eval_Result;
         end if;
      end Resolve_Filename;

   begin

      --  The new redirection states inherits the unmodifed file
      --  descriptors from the old state.

      New_States := Old_States;

      --  We are not allowed to close file descriptor we are inheriting
      if not In_Place then
         for J in 0 .. New_States'Last loop
            New_States (J).Can_Be_Closed := False;
         end loop;
      end if;

      for J in 1 .. Length (Redirections) loop

         declare
            C : constant Redirection := Element (Redirections, J);

         begin
            case C.Kind is
               when OPEN_READ =>
                  Open_FD (C.Open_Target,
                           Resolve_Filename (C.Filename),
                           Write => False);

               when OPEN_WRITE =>
                  Open_FD (C.Open_Target,
                           Resolve_Filename (C.Filename));

               when OPEN_APPEND =>
                  Open_FD (C.Open_Target,
                           Resolve_Filename (C.Filename),
                           Append => True);

               when DUPLICATE =>

                  declare
                     FD_Str : constant String :=
                       Eval_String_Unsplit
                         (State, Get_Token_String (C.Source),
                          Has_Command_Subst => Has_Command_Subst);
                     Source_FD : Integer := 0;
                     Is_Valid  : Boolean := False;

                  begin
                     To_Integer (FD_Str, Source_FD, Is_Valid);
                     if Is_Valid and then
                       New_States (Source_FD).Fd /= Invalid_FD
                     then
                        GNAT.Task_Lock.Lock;
                        New_States (C.Dup_Target).Fd :=
                          Dup (New_States (Source_FD).Fd);
                        OS.Exec.Set_Close_On_Exec
                          (New_States (C.Dup_Target).Fd, True, Success);
                        New_States (C.Dup_Target).Can_Be_Closed := True;
                        GNAT.Task_Lock.Unlock;
                     else
                        Has_Errors := True;
                        exit;
                     end if;
                  end;
               when IOHERE =>
                  declare
                     Fd     : File_Descriptor;
                     Name   : Temp_File_Name;
                     Result : Integer;
                     pragma Warnings (Off, Result);
                     Result_String : aliased String :=
                       (if C.Expand
                        then Eval_String_Unsplit
                          (State,
                           Get_Token_String (C.Content),
                           IOHere => True,
                           Has_Command_Subst => Has_Command_Subst)
                        else Get_Token_String (C.Content));
                  begin
                     Create_Temp_File (Fd, Name);
                     Result := Write
                       (Fd, Result_String'Address, Result_String'Length);
                     Close (Fd);

                     Open_FD (C.Doc_Target,
                              Name,
                              Write           => False,
                              Delete_On_Close => True);
                  end;
               when others =>
                  null;
            end case;
         end;
      end loop;

      --  All went well, so we can now push the new states.
      --  Ada.Text_IO.Put_Line (New_States (2).Fd'Img);

      if Has_Errors then
         Put (State, 2, "bad redirections" & ASCII.LF);
         return False;
      end if;

      GNAT.Task_Lock.Lock;

      if In_Place then
         for I in 0 .. New_States'Last loop
            --  Close the current redirection file descriptor only if it was
            --  not the same as the one we want to restore and if it is not
            --- stdin, stdout or stderr
            if Old_States (I).Can_Be_Closed and then
              Integer (Old_States (I).Fd) /= Integer (New_States (I).Fd)
            then
               Close (Old_States (I).Fd);
               if Old_States (I).Delete_On_Close then
                  Delete_File (Old_States (I).Filename.all, Success);
               end if;
            end if;
         end loop;
      end if;
      State.Redirections := New_States;

      GNAT.Task_Lock.Unlock;
      return True;
   end Apply_Redirections;

   ---------
   -- Put --
   ---------

   procedure Put (S : Shell_State; IO : Integer; Str : String) is
      Size   : constant Integer := Str'Length;
      S_Copy : aliased String := Str;
      N : Integer;
      pragma Warnings (Off, N);
   begin
      N := Write (S.Redirections (IO).Fd, S_Copy'Address, Size);
   end Put;

   ----------
   -- Read --
   ----------

   function Read (S : Shell_State; IO : Integer) return String is
      use Ada.Strings.Unbounded;
      Buffer : String (1 .. 4096);
      N : Integer;
      Result : Unbounded_String := To_Unbounded_String ("");
   begin

      loop
         N := Read (S.Redirections (IO).Fd, Buffer'Address, 4096);
         if N > 0 then
            Result := Result & Buffer (1 .. N);
         end if;
         exit when N < 4096;
      end loop;
      return To_String (Result);
   end Read;

   function Read (S : Shell_State; IO : Integer) return Character is
      Buffer : String (1 .. 2);
      N : Integer;
   begin
      N := Read (S.Redirections (IO).Fd, Buffer'Address, 1);
      if N > 0 then
         return Buffer (1);
      else
         return ASCII.EOT;
      end if;
   end Read;

   -------------------------
   -- Read_Pipe_And_Close --
   -------------------------

   function Read_Pipe_And_Close
     (S        : in out Shell_State;
      Input_Fd : File_Descriptor)
      return String
   is
      pragma Unreferenced (S);
      use Ada.Strings.Unbounded;
      Buffer : aliased String (1 .. 32000);
      --  ??? Get rid of this hard-coded limitation.
      N : Integer;
      Result_Str : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         N := Read (Input_Fd, Buffer'Address, 32000);
         if N > 0 then
            Result_Str := Result_Str & Buffer (1 .. N);
         end if;
         exit when N = 0;
      end loop;
      GNAT.Task_Lock.Lock;
      Close (Input_Fd);
      GNAT.Task_Lock.Unlock;
      return To_String (Result_Str);
   end Read_Pipe_And_Close;

   -----------------
   -- Set_Pipe_In --
   -----------------

   procedure Set_Pipe_In
     (S : in out Shell_State; Input_Fd : File_Descriptor)
   is
   begin
      --  Store the current states and then pop it, allowing us to access
      --  the previous states in order to modify the current one.  Once
      --  all the modifications to the current states have been made
      --  based on the previous states, we will pop the current states
      --  back.
      S.Redirections (-2) := S.Redirections (0);
      S.Redirections (0) := (Input_Fd, null, False, True);
      --  S.Redirections (-1) := S.Redirections (1);
      --  Restore Stdout
      --  S.Redirections (1) := Tmp2;

      --  Set_Close_On_Exec (S.Redirections (-1).Fd);

   end Set_Pipe_In;

   ------------------
   -- Set_Pipe_Out --
   ------------------

   procedure Set_Pipe_Out (S : in out Shell_State) is
      Pipe   : aliased Pipe_Type;
      Result : Integer;
      Success : Boolean;
      pragma Warnings (Off, Success);
   begin

      Result := Create_Pipe (Pipe'Access);

      if Result /= 0 then
         --  If we are not able to open the pipe, just exit from the current
         --  shell script as we don't know what are the consequences of such
         --  failure
         Shell_Exit (S, 127);
      end if;

      --  Save stdout
      S.Redirections (-1) := S.Redirections (1);
      S.Redirections (1)  := (Pipe.Output, null, False, True);

      --  Keep the other side of the pipe
      S.Redirections (-2) := (Pipe.Input, null, False, True);

      OS.Exec.Set_Close_On_Exec (Pipe.Output, True, Success);
      OS.Exec.Set_Close_On_Exec (Pipe.Input, True, Success);
      --  Needed ?
   end Set_Pipe_Out;

   ------------------------------
   -- Get_Current_Redirections --
   ------------------------------

   function Get_Descriptors (State : Shell_State) return Shell_Descriptors is
   begin
      return State.Redirections;
   end Get_Descriptors;

   --  function Get_Current_Redirections return Shell_Descriptors is
   --  begin
   --     return Redirection_Stack (Redirection_Pos);
   --  end Get_Current_Redirections;

   -------------
   -- Warning --
   -------------

   procedure Warning (S : Shell_State; Msg : String) is
   begin
      Put (S, 2, "[warning] " & Msg & ASCII.LF);
   end Warning;

end Posix_Shell.Variables.Output;
