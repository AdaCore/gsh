with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with Posix_Shell.Lexer; use Posix_Shell.Lexer;

with Posix_Shell.Parser; use Posix_Shell.Parser;
with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
with Posix_Shell.Tree.Dumps; use Posix_Shell.Tree.Dumps;
with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Opts; use Posix_Shell.Opts;
with Posix_Shell; use Posix_Shell;

---------
-- GSH --
---------

function GSH return Integer is
   N : Node_Id;

   Status : Integer := 0;

   Success : Boolean;

begin
   Import_Environment;

   declare
      Current_Dir : constant String := Current_Working_Directory;
   begin
      --  Reset PWD and OLDPWD in order to avoid inheriting the values
      --  from the parent process.
      Set_Var_Value ("PWD", Current_Dir, True);
      Set_Var_Value ("OLDPWD", Current_Dir, True);
      Set_Var_Value ("IFS", " " & ASCII.HT & ASCII.LF);
      Set_Var_Value ("PATH_SEPARATOR", ":");
      --  Disable auto expansion of parameters by the cygwin programs
      declare
         Cygwin : constant String := Get_Var_Value ("CYGWIN");
      begin
         if Cygwin = "" then
            Set_Var_Value ("CYGWIN", "noglob", True);
         else
            Set_Var_Value ("CYGWIN", Get_Var_Value ("CYGWIN") & " noglob",
                           True);
         end if;
      end;

      --  Set the last exit status to zero, so that the first command
      --  in the script can access it (if the first command is "echo $?",
      --  for instance).
      Save_Last_Exit_Status (0);

      --  Process the command line.
      Process_Command_Line (Success);
      if not Success then
         return 2;
      end if;

      --  Set the positional parameters.
      Set_Positional_Parameters (Script_Arguments);

      begin
         if not Run_Command then
            N := Parse_File (Script_Name);
         else
            N := Parse_String (Script_Name);
         end if;
      exception
         when Buffer_Read_Error =>
            --  The error reporting has already been done by
            --  Get_Buffer_From_File. So just abort with exit code 127.
            return 127;
      end;

      if N /= 0 then
         if Dump_Node_Table then
            Dump (N);
         end if;

         if Do_Script_Evaluation then
            begin
               Status := Eval (N);
            exception
               when Shell_Exit_Exception =>
                  Status := Get_Last_Exit_Status;
            end;
         end if;
         Free_Node (N);
      end if;

      return Status;

   exception

      when E : Shell_Syntax_Error | Shell_Non_Implemented |
           Shell_Lexer_Error =>
         Put_Line (Exception_Message (E));
         Put_Line (Symbolic_Traceback (E));
         return 127;


   end;

end GSH;
