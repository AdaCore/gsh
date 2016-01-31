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

with Posix_Shell.States; use Posix_Shell.States;

package Posix_Shell.Tree.Evals is

   procedure Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree);
   --  Eval script represented by Tree in the State context
   --
   --  @param State shell state
   --  @param Tree ast of a script shell

   function Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree)
      return Eval_Result;
   --  Eval script represented by Tree in the State context
   --
   --  @param State shell state
   --  @param Tree ast of a script shell

   function Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree)
      return String;
   --  Eval script represented by Tree in the State context and return its
   --  output
   --
   --  @param State shell state
   --  @param Tree ast of a script shell
   --  @return the standard output of the evaluated script

end Posix_Shell.Tree.Evals;
