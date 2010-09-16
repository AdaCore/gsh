with Posix_Shell.Subst; use Posix_Shell.Subst;
with Ada.Strings.Unbounded;

package body Posix_Shell.Output is

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor;

   procedure Push_Redirections (R : Redirection_Op_Stack; Set_Only : Boolean);

   package Redirection_Stacks is

      --  This package implements a stack of redirection states.
      --  It maintains one such stack as a hidden global entity.

      procedure Push (R : Redirection_States);
      --  Push the given Redirection states to our stack.

      procedure Set (R : Redirection_States);

      function Get_Current_States return Redirection_States;
      --  Return the Redirection_States currently at the top of our stack.

      procedure Pop;
      --  Pop the Redirection_States currently at the top of our stack.
      --  Assumes that there is at least one element in our stack, or
      --  the behavior is undefined (most likely an exception).

   end Redirection_Stacks;
   package body Redirection_Stacks is separate;
   use Redirection_Stacks;

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup, "__gnat_dup");

   ----------------
   -- Close_Pipe --
   ----------------

   procedure Close_Pipe is
      Current_States : constant Redirection_States := Get_Current_States;
      Success : Boolean;
      pragma Warnings (Off, Success);
   begin
      Close (Current_States (0).Fd);
      Delete_File (Current_States (0).Filename.all, Success);
      Pop;
   end Close_Pipe;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put (2, "[error] " & Msg & ASCII.LF);
   end Error;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (IO : Integer) is
   begin
      Put (IO, String'(1 => ASCII.LF));
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

   procedure Pop_Redirections is
      Previous_States  : constant Redirection_States := Get_Current_States;
      Current_States : Redirection_States;
      Success : Boolean;
   begin
      Pop;
      Current_States := Get_Current_States;

      for I in 0 .. Previous_States'Last loop
         --  Close the current redirection file descriptor only if it was not
         --  the same as the one we want to restore and if it is not stdin,
         --  stdout or stderr
         if Integer (Previous_States (I).Fd) /= Integer (Current_States (I).Fd)
           and then Previous_States (I).Fd not in 0 .. 2
         then
            Close (Previous_States (I).Fd);
            if Previous_States (I).Delete_On_Close then
               Delete_File (Previous_States (I).Filename.all, Success);
            end if;
         end if;
      end loop;
   end Pop_Redirections;

   -----------------------
   -- Push_Redirections --
   -----------------------

   procedure Push_Redirections
     (R : Redirection_Op_Stack;
      Set_Only : Boolean)
   is
      Success    : Boolean := False;
      Old_States : constant Redirection_States := Get_Current_States;
      New_States : Redirection_States;
      On_Windows : constant Boolean := Directory_Separator = '\';

      function Is_Null_File (Str : String) return Boolean;
      pragma Inline (Is_Null_File);
      --  return True if the filename correspond the name of the null file
      --  (for example "dev/null" on Unix systems).

      function Resolve_Filename (A : Annotated_String) return String;

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

      function Resolve_Filename (A : Annotated_String) return String
      is
         Eval_Result : constant String := Eval_String_Unsplit (A);
      begin
         if On_Windows and then Eval_Result = "/dev/null" then
            return "NUL";
         else
            return Eval_Result;
         end if;
      end Resolve_Filename;

   begin
      null;
      --  The new redirection states inherits the unmodifed file
      --  descriptors from the old state.
      New_States := Old_States;

      for J in 1 .. R.Top loop
         declare
            C : constant Redirection_Op := R.Ops (J);
         begin
            case C.Cmd is
               when OPEN_READ =>
                  New_States (C.Target_FD).Filename :=
                    new String'(Resolve_Filename (C.Filename));
                  New_States (C.Target_FD).Fd := Open_Read
                    (New_States (C.Target_FD).Filename.all,
                     Binary);
                  New_States (C.Target_FD).Delete_On_Close := False;
               when OPEN_WRITE =>
                  New_States (C.Target_FD).Filename :=
                    new String'(Resolve_Filename (C.Filename));
                  if not Is_Null_File
                    (New_States (C.Target_FD).Filename.all)
                  then
                     Delete_File
                       (New_States (C.Target_FD).Filename.all, Success);
                  end if;
                  New_States (C.Target_FD).Fd := Open_Append
                    (New_States (C.Target_FD).Filename.all, Binary);
                  New_States (C.Target_FD).Delete_On_Close := False;
                  Lseek (New_States (C.Target_FD).Fd, 0, 2);
               when OPEN_APPEND =>
                  New_States (C.Target_FD).Filename :=
                    new String'(Resolve_Filename (C.Filename));
                  New_States (C.Target_FD).Fd := Open_Append
                    (New_States (C.Target_FD).Filename.all, Binary);
                  New_States (C.Target_FD).Delete_On_Close := False;
                  Lseek (New_States (C.Target_FD).Fd, 0, 2);
               when DUPLICATE =>
                  New_States (C.Target_FD).Fd :=
                    Dup (New_States (C.Source_FD).Fd);
               when IOHERE =>
                  declare
                     Fd : File_Descriptor;
                     Name : Temp_File_Name;
                     Result : Integer;
                     pragma Warnings (Off, Result);
                     Result_String : aliased String :=
                       Eval_String_Unsplit (C.Filename);
                  begin
                     Create_Temp_File (Fd, Name);
                     Result := Write
                       (Fd, Result_String'Address, Result_String'Length);
                     Close (Fd);

                     New_States (C.Target_FD).Filename := new String'(Name);
                     New_States (C.Target_FD).Fd := Open_Read
                       (New_States (C.Target_FD).Filename.all,
                        Binary);
                     New_States (C.Target_FD).Delete_On_Close := True;
                  end;
               when others =>
                  null;
            end case;
         end;
      end loop;

      --  All went well, so we can now push the new states.
      if Set_Only then
         Set (New_States);
      else
         Push (New_States);
      end if;
   end Push_Redirections;

   procedure Push_Redirections (R : Redirection_Op_Stack) is
   begin
      Push_Redirections (R, False);
   end Push_Redirections;

   ---------
   -- Put --
   ---------

   procedure Put (IO : Integer; S : String) is
      Size   : constant Integer := S'Length;
      S_Copy : aliased String := S;
      N : Integer;
      pragma Warnings (Off, N);
      Current_States : constant Redirection_States := Get_Current_States;
   begin
      N := Write (Current_States (IO).Fd, S_Copy'Address, Size);
   end Put;

   ----------
   -- Read --
   ----------

   function Read (IO : Integer) return String is
      use Ada.Strings.Unbounded;
      Buffer : String (1 .. 4096);
      N : Integer;
      Current_States : constant Redirection_States := Get_Current_States;
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         N := Read (Current_States (IO).Fd, Buffer'Address, 4096);
         if N > 0 then
            Result := Result & Buffer (1 .. N);
         end if;
         exit when N < 4096;
      end loop;
      return To_String (Result);
   end Read;

   function Read (IO : Integer) return Character is
      Buffer : String (1 .. 2);
      N : Integer;
      Current_States : constant Redirection_States := Get_Current_States;
   begin
      N := Read (Current_States (IO).Fd, Buffer'Address, 1);
      if N > 0 then
         return Buffer (1);
      else
         return ASCII.EOT;
      end if;
   end Read;

   -------------------------
   -- Read_Pipe_And_Close --
   -------------------------

   function Read_Pipe_And_Close return String is
      Result_Str : aliased String (1 .. 32000);
      --  ??? Get rid of this hard-coded limitation.
      Result     : Integer;
      Success : Boolean;
      Current_States : Redirection_States := Get_Current_States;
   begin
      Close (Current_States (-1).Fd);
      Current_States (-2).Fd :=
        Open_Read (Current_States (-1).Filename.all, Binary);
      Result := Read (Current_States (-2).Fd, Result_Str'Address, 32000);
      Close (Current_States (-2).Fd);
      Delete_File (Current_States (-1).Filename.all, Success);
      Pop;

      return Result_Str (1 .. Result);
   end Read_Pipe_And_Close;

   -----------------
   -- Set_Pipe_In --
   -----------------

   procedure Set_Pipe_In is
      Current_States  : Redirection_States;
      Previous_States : Redirection_States;
   begin
      --  Store the current states and then pop it, allowing us to access
      --  the previous states in order to modify the current one.  Once
      --  all the modifications to the current states have been made
      --  based on the previous states, we will pop the current states
      --  back.
      Current_States := Get_Current_States;
      Pop;
      Previous_States := Get_Current_States;

      Close (Current_States (-1).Fd);
      Current_States (1) := Previous_States (1);

      Current_States (0).Fd := Open_Read
        (Current_States (-1).Filename.all, Binary);
      Current_States (0).Filename := Current_States (-1).Filename;

      --  Set_Close_On_Exec (Current_States (-2).Fd, False, Success);

      Push (Current_States);
   end Set_Pipe_In;

   ------------------
   -- Set_Pipe_Out --
   ------------------

   procedure Set_Pipe_Out is
      --  Pipe    : aliased Pipe_Type;
      Pipe_Out  : File_Descriptor;
      Pipe_Filename : Temp_File_Name;
      Old_States : constant Redirection_States := Get_Current_States;
      New_States : Redirection_States;
   begin

      --  Result := Create_Pipe (Pipe'Access);

      --  if Result /= 0 then
         --  If we are not able to open the pipe, just exit from the current
         --  shell script as we don't know what are the consequences of such
         --  failure
      --   Shell_Exit (127);
         --  end if;

      Create_Temp_File (Pipe_Out, Pipe_Filename);
      --  Close (Pipe_Out);
      --  Pipe_Out := Create_File (Pipe_Filename, Binary);
      New_States := Old_States;
      --  New_States (-2).Fd := -1;
      --  New_States (-2).Filename := null;
      New_States (-1).Fd := Pipe_Out;
      New_States (-1).Filename :=
        new String'(GNAT.OS_Lib.Normalize_Pathname
                    (Pipe_Filename));
      New_States (1).Fd := Pipe_Out;
      New_States (1).Filename :=
        new String'(GNAT.OS_Lib.Normalize_Pathname
                    (Pipe_Filename));
      --  Set_Close_On_Exec (Pipe.Input, True, Success);

      Push (New_States);
   end Set_Pipe_Out;

   procedure Set_Redirections (R : Redirection_Op_Stack) is
   begin
      Push_Redirections (R, True);
   end Set_Redirections;
   ------------------------------
   -- Get_Current_Redirections --
   ------------------------------

   function Get_Current_Redirections return Redirection_States
     renames Get_Current_States;
   --  function Get_Current_Redirections return Redirection_States is
   --  begin
   --     return Redirection_Stack (Redirection_Pos);
   --  end Get_Current_Redirections;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      Put (2, "[warning] " & Msg & ASCII.LF);
   end Warning;

end Posix_Shell.Output;
