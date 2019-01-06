------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                              Sh.Utils                           --
--                                                                          --
--                                 S p e c                                  --
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

with GNAT.Strings; use GNAT.Strings;
with Sh.States; use Sh.States;

package Sh.Utils is

   procedure To_Integer (S : String; V : out Integer; Valid : out Boolean);
   --  If the string "S" is a valid decimal representation of a number,
   --  return its value in "V", and set Valid to True. Otherwise, set
   --  "Valid" to False (in which case, the value of V is undefined).

   procedure To_LongLong (S : String;
                          V : out Long_Long_Integer;
                          Valid : out Boolean);
   --  If the string "S" is a valid decimal representation of a number,
   --  return its value in "V", and set Valid to True. Otherwise, set
   --  "Valid" to False (in which case, the value of V is undefined).

   function To_Integer (B : Boolean) return Integer;
   --  Return the shell-version of True and False: If B is True, then
   --  return zero, else return 1.

   function Integer_Not (Value : Integer) return Integer;
   --  Performs the shell equivalent of a not: If Value is zero, then
   --  return 1. Else, return 0.

   function Locate_Exec
     (S         : Shell_State;
      Exec_Name : String)
      return String_Access;
   --  This function correspond to the one implemented in GNAT.OS_Lib with the
   --  difference that it does not replace path components which are links

   function Readline (Prompt : String) return String;

end Sh.Utils;
