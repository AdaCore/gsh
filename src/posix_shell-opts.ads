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
     (State : Shell_State_Access;
      B : out Buffer_Access;
      Status : out Integer;
      Is_Interactive : out Boolean);
   --  Process all the switches and arguments on the command line.
   --  This also verifies that a non-empty script filename is provided.
   --  If an error is detected, then Status is set to an integer different
   --  from 0 and a message is printed in the standard error.
   --  When the returned value of Is_Interactive is True then reading on stdin
   --  is delegated to the main function in gsh.adb. Otherwise the script to
   --  execute is returned in the buffer B.
   --  Note also that this function is in charge of reading the gsh config file
   --  when necessary.

end Posix_Shell.Opts;
