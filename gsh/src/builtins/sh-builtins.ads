------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins                               --
--                                                                          --
--                                 S p e c                                  --
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

with Sh.States; use Sh.States;
with C.Strings; use C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Sh.Builtins is

   type Builtin_Function
     is access function
       (S : in out Shell_State; Args : String_List) return Eval_Result;
   --  The signature of a function implementing a given builtin.

   function Is_Builtin (Cmd : String) return Boolean;
   --  Return True if Cmd is a known builtin.

   function Execute_Builtin
     (S    : in out Shell_State;
      Cmd  : String;
      Args : CList)
      return Eval_Result;
   --  Execute the given builtin.

end Sh.Builtins;
