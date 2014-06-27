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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Variables;  use Posix_Shell.Variables;
with Token_Lists; use Token_Lists;

package Posix_Shell.Subst is

   function Eval_String
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
      S  : Token_List)
      return String_List;

end Posix_Shell.Subst;
