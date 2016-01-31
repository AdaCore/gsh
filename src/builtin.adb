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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Sh.States; use Sh.States;
with Sh.Builtins; use Sh.Builtins;
with Sh; use Sh;

-------------
-- Builtin --
-------------

function Builtin return Integer is
   Status        : Eval_Result;
   State         : Shell_State;
begin
   --  Import into our state the current process environment
   Import_Environment (State);

   declare
      Current_Dir : constant String := Get_Current_Dir (State, True);
      Arguments   : String_List (1 .. Argument_Count);
      Cmd         : constant String := Base_Name
        (Ada.Command_Line.Command_Name);
   begin
      --  Reset PWD and OLDPWD in order to avoid inheriting the values
      --  from the parent process.
      Set_Var_Value (State, "PWD", Current_Dir, True);
      Set_Var_Value (State, "OLDPWD", Current_Dir, True);
      Set_Var_Value (State, "IFS", " " & ASCII.HT & ASCII.LF);
      Set_Var_Value (State, "PATH_SEPARATOR", ":");

      for Index in 1 .. Argument_Count loop
         Arguments (Index) := new String'(Ada.Command_Line.Argument (Index));
      end loop;

      Status := Execute_Builtin (State, Cmd, Arguments);

      case Status.Kind is
         when RESULT_STD =>
            return Status.Status;
         when others =>
            return 127;
      end case;

   exception
      when others =>
         Put_Line (Cmd & " crashed");
         return 127;
   end;

end Builtin;
