with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

--  with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with Posix_Shell.Lexer; use Posix_Shell.Lexer;

with Posix_Shell.Parser; use Posix_Shell.Parser;
with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
--  with Posix_Shell.Tree.Dumps; use Posix_Shell.Tree.Dumps;
with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Opts; use Posix_Shell.Opts;
with Posix_Shell.Builtins; use Posix_Shell.Builtins;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Posix_Shell; use Posix_Shell;

---------
-- GSH --
---------

function GSH return Integer is
   T : Shell_Tree_Access;

   Status : Integer := 0;

   State : constant Shell_State_Access := new Shell_State;

   Script_Buffer : Buffer_Access := null;

begin
   Register_Default_Builtins;
   Import_Environment (State.all);

   declare
      Current_Dir : constant String := Get_Current_Dir (State.all, True);
      Is_Interactive : Boolean;
   begin
      --  Reset PWD and OLDPWD in order to avoid inheriting the values
      --  from the parent process.
      Set_Var_Value (State.all, "PWD", Current_Dir, True);
      Set_Var_Value (State.all, "OLDPWD", Current_Dir, True);
      Set_Var_Value (State.all, "IFS", " " & ASCII.HT & ASCII.LF);
      Set_Var_Value (State.all, "PATH_SEPARATOR", ":");
      --  Disable auto expansion of parameters by the cygwin programs
      declare
         Cygwin : constant String := Get_Var_Value (State.all, "CYGWIN");
      begin
         if Cygwin = "" then
            Set_Var_Value (State.all, "CYGWIN", "noglob", True);
         else
            Set_Var_Value (State.all, "CYGWIN",
                           Get_Var_Value (State.all, "CYGWIN") & " noglob",
                           True);
         end if;
      end;

      --  Set the last exit status to zero, so that the first command
      --  in the script can access it (if the first command is "echo $?",
      --  for instance).
      Save_Last_Exit_Status (State.all, 0);

      --  Process the command line.
      Process_Command_Line (State, Script_Buffer, Status, Is_Interactive);

      if Status /= 0 then
         return Status;
      end if;

      loop
         if Is_Interactive then
            Put (State.all, 1, "$ ");
            declare
               use Ada.Strings.Unbounded;
               use GNAT.OS_Lib;
               Result : Unbounded_String := To_Unbounded_String ("");
               Buffer : String (1 .. 4096);
               Buffer_Size : constant Integer := 4096;
               N : Integer;
            begin
               loop
                  N := Read (Standin, Buffer'Address, Buffer_Size);
                  Append (Result, Buffer (1 .. N));
                  exit when N < Buffer_Size;
               end loop;
               Deallocate (Script_Buffer);
               Script_Buffer :=
                 new Token_Buffer'(New_Buffer (To_String (Result)));
            end;
         end if;
         T := Parse_Buffer (Script_Buffer);

         if Do_Script_Evaluation then
            begin
               Eval (State, T.all);
               Status := Get_Last_Exit_Status (State.all);
            exception
               when Shell_Exit_Exception =>
                  Status := Get_Last_Exit_Status (State.all);
                  exit;
               when Shell_Return_Exception =>
                  Put
                    (State.all, 2,
                     "return: can only `return' from a " &
                       "function or sourced script" & ASCII.LF);
                  Save_Last_Exit_Status (State.all, 1);
                  Status := 1;

            end;
         end if;
         Free_Node (T);
         if not Is_Interactive then
            exit;
         end if;
      end loop;

      return Status;

   exception

      when E : Shell_Syntax_Error | Shell_Non_Implemented |
           Shell_Lexer_Error =>
         Put_Line (Exception_Message (E));
         --  Put_Line (Symbolic_Traceback (E));
         return 127;


   end;

end GSH;
