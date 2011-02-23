with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

--  with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with Posix_Shell.Lexer; use Posix_Shell.Lexer;

with Posix_Shell.Parser; use Posix_Shell.Parser;
with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
--  with Posix_Shell.Tree.Dumps; use Posix_Shell.Tree.Dumps;
with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Opts; use Posix_Shell.Opts;
with Posix_Shell; use Posix_Shell;

---------
-- GSH --
---------

function GSH return Integer is
   T : Shell_Tree_Access;

   Status : Integer := 0;

   Success : Boolean;
   State : constant Shell_State_Access := new Shell_State;

begin
   Import_Environment (State.all);

   declare
      Current_Dir : constant String := Get_Current_Dir (State.all, True);
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
      Process_Command_Line (Success);
      if not Success then
         return 2;
      end if;

      --  Set the positional parameters.
      Set_Positional_Parameters (State.all, Script_Arguments);

      begin
         if not Run_Command then
            T := Parse_File (Script_Name);
         else
            T := Parse_String (Script_Name);
         end if;
      exception
         when Buffer_Read_Error =>
            --  The error reporting has already been done by
            --  Get_Buffer_From_File. So just abort with exit code 127.
            return 127;
      end;

      --  if Dump_Node_Table then
      --    Dump (N);
      --  end if;

      if Do_Script_Evaluation then
         begin
            Eval (State, T.all);
            Status := Get_Last_Exit_Status (State.all);
         exception
            when Shell_Exit_Exception =>
               Status := Get_Last_Exit_Status (State.all);
         end;
      end if;
      Free_Node (T);

      return Status;

   exception

      when E : Shell_Syntax_Error | Shell_Non_Implemented |
           Shell_Lexer_Error =>
         Put_Line (Exception_Message (E));
         --  Put_Line (Symbolic_Traceback (E));
         return 127;


   end;

end GSH;
