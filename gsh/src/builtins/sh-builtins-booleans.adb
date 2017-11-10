------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Booleans                      --
--                                                                          --
--                                 B o d y                                  --
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

package body Sh.Builtins.Booleans is

   ------------------
   -- True_Builtin --
   ------------------

   function True_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result
   is
      pragma Unreferenced (Args);
      pragma Unreferenced (S);
   begin
      return (RESULT_STD, 0);
   end True_Builtin;

   -------------------
   -- False_Builtin --
   -------------------

   function False_Builtin
     (S : in out Shell_State;
      Args : String_List)
      return Eval_Result
   is
      pragma Unreferenced (Args);
      pragma Unreferenced (S);
   begin
      return (RESULT_STD, 1);
   end False_Builtin;

end Sh.Builtins.Booleans;
