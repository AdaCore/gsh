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

with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
with Posix_Shell.Tree; use Posix_Shell.Tree;
package body Posix_Shell.Functions is

   ----------------------
   -- Execute_Function --
   ----------------------

   procedure Execute_Function
     (State : in out Shell_State;
      Name  : String;
      Args : String_List)
   is
      Function_Tree : constant Shell_Tree :=
        Get_Function (State, Name);
      Saved_Pos_Params : constant Pos_Params_State :=
        Get_Positional_Parameters (State);

   begin
      Set_Positional_Parameters (State, Args, False);

      Eval (State, Function_Tree);
      Restore_Positional_Parameters (State, Saved_Pos_Params);

   exception
      when Shell_Exit_Exception =>
         Restore_Positional_Parameters (State, Saved_Pos_Params);
         raise;
   end Execute_Function;

end Posix_Shell.Functions;
