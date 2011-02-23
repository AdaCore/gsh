with Posix_Shell.Builtins; use Posix_Shell.Builtins;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Opts; use Posix_Shell.Opts;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Functions; use Posix_Shell.Functions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Text_IO;
with GNAT.Task_Lock;

package body Posix_Shell.Commands_Preprocessor is

   Executables_Extensions : constant array (1 .. 4) of String_Access :=
     (new String'(""),
      new String'(".vxe"),
      new String'(".exe"),
      new String'(".out"));
   --  ??? The ".exe" extension is only valid on Windows. We should
   --  ??? probably change this constant into a function.
   --  ??? I don't understand the ".vxe" or ".out" extensions, though.

   function Find_Exec (Cmd : String) return String_Access;
   function Get_Launcher (Filename : String_Access) return String_List;

   function Find_Exec (Cmd : String) return String_Access is
      Exec_Path : String_Access := null;
   begin
      for I in Executables_Extensions'Range loop
         Exec_Path := Locate_Exec_On_Path
           (Cmd & Executables_Extensions (I).all);
         exit when Exec_Path /= null;
      end loop;
      return Exec_Path;
   end Find_Exec;

   function Get_Launcher (Filename : String_Access) return String_List is
      Fd           : File_Descriptor;
      Buffer       : aliased String (1 .. 256);
      Buffer_Last  : Natural := 0;
      Result       : String_List (1 .. 32);
      Result_Last  : Natural := 0;
   begin
      Fd := Open_Read (Filename.all, Binary);
      --  Here we read only 256 characters. That should be sufficient
      Buffer_Last := Read (Fd, Buffer'Address, 256);
      Close (Fd);

      if Buffer_Last >= 2 and then Buffer (1 .. 2) = "#!" then
         --  If the first two character of the file to execute are #! then
         --  this is a script. Find which program should be used to launch it
         declare
            Token_First : Natural := 0;
         begin

            for J in 3 .. Buffer_Last loop
               --  First check for token delimiters
               if Token_First > 0
                 and then (Buffer (J) = ' ' or else Buffer (J) = ASCII.LF)
               then
                  Result_Last := Result_Last + 1;
                  if Result_Last = 1 then
                     --  Don't trust paths in '#!". Usually they are unix paths
                     --  just ignore it for the time being
                     Result (Result_Last) :=
                       new String'(Base_Name (Buffer (Token_First .. J - 1)));
                  else
                     Result (Result_Last) :=
                       new String'(Buffer (Token_First .. J - 1));
                  end if;
                  Token_First := 0;
               end if;

               case Buffer (J) is
                  when ' '      => null; -- skip spaces
                  when ASCII.LF => exit; -- a LF mark the end of the parsing
                  when others =>
                     if Token_First = 0 then
                        Token_First := J;
                     end if;
               end case;
            end loop;

         end;
      end if;

      Result_Last := Result_Last + 1;
      Result (Result_Last) := Filename;

      --  Check if the command to spawn is either bash or sh. Replace by gsh
      --  call.
      declare
         Original_Cmd : constant String := Filename.all;
         Cmd : constant String := Base_Name (Result (1).all, ".exe");
      begin
         if Cmd = "bash" or Cmd = "sh" then
            Free (Result (1));
            Result (1) := new String'("gsh");
         end if;

         if Result (1).all /= Original_Cmd then
            --  resolve now the launcher path
            declare
               Tmp : constant String_Access := Find_Exec (Result (1).all);
            begin
               Free (Result (1));
               Result (1) := Tmp;
            end;

         end if;
      end;

      return Result (1 .. Result_Last);

   end Get_Launcher;

   function Run (S : Shell_State_Access;
                 Cmd : String;
                 Args : String_List;
                 Env : String_List)
                 return Integer
   is
      Exec_Path : String_Access := null;
      Exit_Status : Integer;
   begin

      if Debug_Mode then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "+ " & Cmd);
         for I in Args'Range loop
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             " '" & Args (I).all & "'");
         end loop;
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end if;

      --  Handle builtins first.
      if Is_Builtin (Cmd) then
         Exit_Status := Execute_Builtin (S, Cmd, Args);
         Save_Last_Exit_Status (S.all, Exit_Status);
         return Exit_Status;
      end if;

      --  Next, is this a function?
      if Is_Function (Cmd) then
         Execute_Function (S, Cmd, Args);
         return Get_Last_Exit_Status (S.all);
      end if;

      --  This command can only be an executable. See if we can
      --  locate an executable using the various possible filename
      --  extensions.
      for I in Executables_Extensions'Range loop
         GNAT.Task_Lock.Lock;
         declare
            Real_Current_Dir : constant String := Get_Current_Dir;
         begin
            Change_Dir (Get_Current_Dir (S.all));
            Exec_Path := Locate_Exec_On_Path
              (Cmd & Executables_Extensions (I).all);
            Change_Dir (Real_Current_Dir);
         end;
         GNAT.Task_Lock.Unlock;
         exit when Exec_Path /= null;
      end loop;

      if Exec_Path = null then
         Put (S.all, 2, Cmd & ": command not found"); New_Line (S.all, 2);
         return 127;
      end if;

      declare
         Launcher : String_List := Get_Launcher (Exec_Path);
         Cmd_Line : constant String_List := Launcher & Args;
      begin
         if Cmd_Line (Cmd_Line'First) = null then
            Put (S.all, 2, Cmd & ": can't launch program");
            New_Line (S.all, 2);
            Exit_Status := 127;
         else

            Exit_Status := Blocking_Spawn
              (Cmd_Line,
               Get_Current_Dir (S.all),
               Env,
               Get_Fd (S.all, 0),
               Get_Fd (S.all, 1),
               Get_Fd (S.all, 2));
         end if;
         for J in Launcher'Range loop
            Free (Launcher (J));
         end loop;
      exception
         when Program_Error =>
            for J in Launcher'Range loop
               Free (Launcher (J));
            end loop;
            Exit_Status := 127;
      end;

      return Exit_Status;
   end Run;

end Posix_Shell.Commands_Preprocessor;
