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

package Lua.Utils is

   function Call_Function (State : Lua_State; Name : String) return String;
   --  call a lua function that takes no parameter and return a string

   function Call_Function
     (State : Lua_State;
      Name  : String;
      Arg   : String)
      return String;
   --  call a lua function that takes a string as parameter and return a
   --  string

   generic
      with function Action (Data : String) return String;
   function Lua_String_Filter (State : Lua_State) return Integer;
   pragma Convention (C, Lua_String_Filter);

end Lua.Utils;
