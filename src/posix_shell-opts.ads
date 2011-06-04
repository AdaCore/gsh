with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.Opts is

   Dump_Node_Table : Boolean := False;
   --  If true, then dump after having parsed the script, but before
   --  evaluating it.
   --
   --  Set to True with the '-n' switch.

   Do_Script_Evaluation : Boolean := True;
   --  If true, then the script will only be parsed, and the evaluation
   --  phase will not be performed.
   --
   --  Set to False with the '-n' switch.

   Debug_Lexer : Boolean := False;

   procedure Process_Command_Line
     (State : Shell_State_Access; B : out Buffer_Access; Status : out Integer);
   --  Process all the switches and arguments on the command line.
   --  This also verifies that a non-empty script filename is provided.
   --
   --  If an error is detected, then Success is set to False and
   --  an error message describing the problem is printed on standard
   --  error.

end Posix_Shell.Opts;
