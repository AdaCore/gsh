------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

package Sh.String_Utils is

   function Starts_With (S : String; Sub : String) return Boolean;
   --  return True if S starts with Sub, False otherwise.

   function Last_Lines
     (S : String; Size : Natural) return String;
   --  return the last Size lines of S

   function From_Line
     (S : String; LineNo : Natural) return String;
   --  return the substring starting at line LineNo ending at end of S

   function To_Line
     (S : String; LineNo : Natural) return String;
   --  return the substrubg till line LIneNo of S

   function Is_Valid_Variable_Name (Name : String) return Boolean;
   --  Return  True if Name is, from a syntactic point of view, a valid
   --  variable name, False otherwise.

   --  function Get_Var_Value (Name : String) return String_Access;
   --  If Name is a defined variable name, then return a pointer
   --  to its current value.  Return null otherwise.
   --
   --  The pointer returned should be deallocated after use.

   function To_String (I : Integer) return String;
   --  Convert an Integer to a string without leading spaces

   function Is_Natural (S : String) return Boolean;

   function Strip_CR (Str : String) return String;
   --  This function removes all CR from a given string STR

end Sh.String_Utils;
