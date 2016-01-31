------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with Sh.Subst; use Sh.Subst;
with Sh.Utils; use Sh.Utils;
with Ada.Strings.Unbounded;
pragma Warnings (Off);
with System.CRTL;
pragma Warnings (On);
with GNAT.Task_Lock;
with OS.FS;

package body Sh.States.IO is

   -------------------------
   -- Get_File_Descriptor --
   -------------------------

   function Get_File_Descriptor
     (State : Shell_State;
      IO    : Integer)
      return OS.FS.File_Descriptor
   is
   begin
      return State.Redirections (IO).Fd;
   end Get_File_Descriptor;

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

   -------------------------
   -- Restore_Descriptors --
   -------------------------

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
               OS.FS.Close (Previous (I).Fd);
               if Previous (I).Delete_On_Close then
                  Delete_File (Previous (I).Filename.all, Success);
               end if;
            end if;
         end loop;
         GNAT.Task_Lock.Unlock;
      end;
   end Restore_Descriptors;

   ------------------------
   -- Apply_Redirections --
   ------------------------

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

      function Resolve_Filename (A : Token) return String;

      procedure Open_FD
         (FD              : Natural;
          Path            : String;
          Mode            : OS.FS.File_Mode;
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
          Mode            : OS.FS.File_Mode;
          Delete_On_Close : Boolean := False)
      is
      begin
         New_States (FD).Filename := new String'(Path);
         New_States (FD).Fd := OS.FS.Open (Path, Mode);
         New_States (FD).Delete_On_Close := Delete_On_Close;
         New_States (FD).Can_Be_Closed := True;
      end Open_FD;

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

      --  If In_Place is set to true it means that the new state replace the
      --  former one and thus takes oownership of file descriptors owned by
      --  previous state. If not In_Place (for example, when creating a new
      --  scope), then the previous state keeps ownership of the file
      --  descriptors we are inheriting.

      if not In_Place then
         for J in 0 .. New_States'Last loop
            New_States (J).Can_Be_Closed := False;
         end loop;
      end if;

      --  Apply the redirection operations the the newly created state
      for J in 1 .. Length (Redirections) loop
         declare
            C : constant Redirection := Element (Redirections, J);
         begin
            case C.Kind is
               when OPEN_READ =>
                  --  Handling of <
                  Open_FD (FD   => C.Open_Target,
                           Path => Resolve_Filename (C.Filename),
                           Mode => OS.FS.Read_Mode);

               when OPEN_WRITE =>
                  --  Handling of >
                  Open_FD (FD   => C.Open_Target,
                           Path => Resolve_Filename (C.Filename),
                           Mode => OS.FS.Write_Mode);

               when OPEN_APPEND =>
                  --  Handling of >>
                  Open_FD (FD   => C.Open_Target,
                           Path => Resolve_Filename (C.Filename),
                           Mode => OS.FS.Append_Mode);

               when DUPLICATE =>
                  --  Handling of >&
                  declare
                     FD_Str : constant String :=
                       Eval_String_Unsplit
                         (State, Get_Token_String (C.Source),
                          Has_Command_Subst => Has_Command_Subst);
                     Source_FD : Integer := 0;
                     Is_Valid  : Boolean := False;
                     use OS.FS;
                  begin
                     To_Integer (FD_Str, Source_FD, Is_Valid);
                     if Is_Valid and then
                       New_States (Source_FD).Fd /= OS.FS.Invalid_FD
                     then
                        New_States (C.Dup_Target).Fd :=
                          OS.FS.Dup (New_States (Source_FD).Fd,
                                     Close_On_Exec => True);
                        New_States (C.Dup_Target).Can_Be_Closed := True;
                     else
                        Has_Errors := True;
                        exit;
                     end if;
                  end;
               when IOHERE =>
                  --  Handling of << and <<-
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
                     --  currently we are creating a temporary file on disk
                     --  it would be better to use shared memory here.
                     Create_Temp_File (Fd, Name);
                     Result := Write
                       (Fd, Result_String'Address, Result_String'Length);
                     Close (Fd);

                     Open_FD (FD              => C.Doc_Target,
                              Path            => Name,
                              Mode            => OS.FS.Read_Mode,
                              Delete_On_Close => True);
                  end;
               when NULL_REDIR =>
                  null;
            end case;
         end;
      end loop;

      if Has_Errors then
         --  In case error has been found, just display a message and return
         --  (???) We are potentially leaking some handlers here
         Put (State, 2, "bad redirections" & ASCII.LF);
         return False;
      end if;

      --  No error was found so push the newly created state as the current
      --  one. In case the redirections are applied in place, do necessary
      --  cleanup to avoid leaking file descriptors.
      GNAT.Task_Lock.Lock;
      if In_Place then
         for I in 0 .. New_States'Last loop
            --  Close the current redirection file descriptor only if it was
            --  not the same as the one we want to restore and if it is not
            --- stdin, stdout or stderr
            if Old_States (I).Can_Be_Closed and then
              Integer (Old_States (I).Fd) /= Integer (New_States (I).Fd)
            then
               OS.FS.Close (Old_States (I).Fd);
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
   begin
      OS.FS.Write (S.Redirections (IO).Fd, Str);
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
         N := OS.FS.Read (S.Redirections (IO).Fd, Buffer);
         if N > 0 then
            Result := Result & Buffer (1 .. N);
         end if;
         exit when N < 4096;
      end loop;

      return To_String (Result);
   end Read;

   ----------
   -- Read --
   ----------

   function Read (S : Shell_State; IO : Integer) return Character is
      Buffer : String (1 .. 1);
      N : Integer;
   begin
      N := OS.FS.Read (S.Redirections (IO).Fd, Buffer);
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
      Input_Fd : OS.FS.File_Descriptor)
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
         N := OS.FS.Read (Input_Fd, Buffer);
         if N > 0 then
            Result_Str := Result_Str & Buffer (1 .. N);
         end if;
         exit when N = 0;
      end loop;
      GNAT.Task_Lock.Lock;
      OS.FS.Close (Input_Fd);
      GNAT.Task_Lock.Unlock;
      return To_String (Result_Str);
   end Read_Pipe_And_Close;

   ---------------------
   --  Set_Descriptor --
   ---------------------

   procedure Set_Descriptor
     (State : in out Shell_State;
      IO    : Integer;
      Fd    : OS.FS.File_Descriptor)

   is
      Success : Boolean;
      Current : Shell_Descriptor := State.Redirections (IO);
   begin
      if Current.Can_Be_Closed then
         OS.FS.Close (Current.Fd);
         if Current.Delete_On_Close then
            Delete_File (Current.Filename.all, Success);
            Free (Current.Filename);
         end if;
      end if;
      State.Redirections (IO) := (Fd, null, False, True);
   end Set_Descriptor;

   ------------------------------
   -- Get_Current_Redirections --
   ------------------------------

   function Get_Descriptors (State : Shell_State) return Shell_Descriptors is
   begin
      return State.Redirections;
   end Get_Descriptors;

   -------------
   -- Warning --
   -------------

   procedure Warning (S : Shell_State; Msg : String) is
   begin
      Put (S, 2, "[warning] " & Msg & ASCII.LF);
   end Warning;

end Sh.States.IO;
