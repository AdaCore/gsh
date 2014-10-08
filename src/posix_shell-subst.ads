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

with Dyn_String_Lists;

with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Posix_Shell.Variables;  use Posix_Shell.Variables;
with Posix_Shell.Tree;       use Posix_Shell.Tree;
with Posix_Shell.List_Pools; use Posix_Shell.List_Pools;

package Posix_Shell.Subst is

   function Eval_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return String_List;

   function Split_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return String_List;

   function Eval_String_Unsplit
     (SS                 : Shell_State_Access;
      S                  : String;
      Case_Pattern       : Boolean := False;
      IOHere             : Boolean := False)
      return String;

   function Eval_String_List
     (SS : Shell_State_Access;
      T  : Shell_Tree;
      S  : Token_List)
      return String_List;

   procedure Split_Arithmetic_String
     (SS                  : Shell_State_Access;
      Str                 : String;
      Previous_Was_Number : in out Boolean;
      Args_List           : in out Dyn_String_Lists.Dyn_String_List);
   --  Split the string Str into a list of elements that are either
   --  numbers, symbole names, operators, or parantheses.
   --  Previous_Was_Number is the boolean that gets and updates accordingly
   --  the 'nature' of the previous token. True when previous token was
   --  a number or a symbole name, False otherwise.

end Posix_Shell.Subst;