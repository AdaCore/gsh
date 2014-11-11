------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2014, AdaCore                   --
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

   procedure Process_Command_Line
     (State : in out Shell_State;
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
