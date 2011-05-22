with GNAT.Strings; use GNAT.Strings;
with Posix_Shell.Variables; use Posix_Shell.Variables;

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

   Run_Command : Boolean := False;

   Debug_Lexer : Boolean := False;

   procedure Process_Command_Line
     (State : Shell_State_Access; Success : out Boolean);
   --  Process all the switches and arguments on the command line.
   --  This also verifies that a non-empty script filename is provided.
   --
   --  If an error is detected, then Success is set to False and
   --  an error message describing the problem is printed on standard
   --  error.

   function Script_Name return String;
   --  Return the name of the script to run.
   --
   --  This function can only be called AFTER Process_Command_Line
   --  has been called and returned a Success. The behavior is unspecified
   --  otherwise.

   function Script_Arguments return String_List;
   --  Return a list of all the arguments to pass to the script
   --  about to be executed.  The result must be deallocated after use
   --  with GNAT.Strings.Free.
   --
   --  This function can only be called AFTER Process_Command_Line
   --  has been called and returned a Success. The behavior is unspecified
   --  otherwise.

end Posix_Shell.Opts;
