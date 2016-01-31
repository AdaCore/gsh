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

with Dyn_String_Lists;

with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Sh.States;  use Sh.States;
with Sh.Tree;       use Sh.Tree;
with Sh.List_Pools; use Sh.List_Pools;

package Sh.Subst is

   function Eval_String
     (SS        : in out Shell_State;
      S         : String;
      Max_Split : Integer := -1)
      return String_List;

   function Split_String
     (SS        : in out Shell_State;
      S         : String;
      Max_Split : Integer := -1)
      return String_List;

   function Eval_String_Unsplit
     (SS                 : in out Shell_State;
      S                  : String;
      Case_Pattern       : Boolean := False;
      IOHere             : Boolean := False;
      Has_Command_Subst  : out Boolean)
      return String;
   --  Do substitution but do not split final result. Case_Pattern or IOHere
   --  must be set to True if in a case pattern or in a IOHere respectively.
   --  Has_Command_Subst is set to true if during the substitution a
   --  command substitution was done (needed to compute assignment exit
   --  status. See Eval_Assign).

   function Eval_String_List
     (SS : in out Shell_State;
      T  : Shell_Tree;
      S  : Token_List)
      return String_List;

   procedure Split_Arithmetic_String
     (SS                  : in out Shell_State;
      Str                 : String;
      Previous_Was_Number : in out Boolean;
      Args_List           : in out Dyn_String_Lists.Dyn_String_List);
   --  Split the string Str into a list of elements that are either
   --  numbers, symbole names, operators, or parantheses.
   --  Previous_Was_Number is the boolean that gets and updates accordingly
   --  the 'nature' of the previous token. True when previous token was
   --  a number or a symbole name, False otherwise.

end Sh.Subst;
