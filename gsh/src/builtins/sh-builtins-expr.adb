------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Expr                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2019, AdaCore                   --
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

with Sh.Subst.Arith;      use Sh.Subst.Arith;
with Sh.States.IO; use Sh.States.IO;
with GNAT.Strings; use GNAT.Strings;

package body Sh.Builtins.Expr is

   -------------------
   --  Expr_Builtin --
   -------------------

   function Expr_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      Args_Copy : constant String_List := As_List (Args);
      -- ??? should be deallocated --
   begin

      declare
         Result : constant String := Eval_Expr (S, Args_Copy);
      begin
         Put (S, 1, Result & ASCII.LF);
         if Result = "0" or else Result = "" then
            return (RESULT_STD, 1);
         else
            return (RESULT_STD, 0);
         end if;
      end;

   exception
      when Expr_Error =>
         Put (S, 2, "invalid expression" & ASCII.LF);
         return (RESULT_STD, 2);
   end Expr_Builtin;

end Sh.Builtins.Expr;
