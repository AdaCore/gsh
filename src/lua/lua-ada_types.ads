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

generic
   Name : String;
   type Ada_Type is private;
package Lua.Ada_Types is

   procedure Push (State : Lua_State; Data : Ada_Type);
   --  Push an Ada_Type value as a UserData Lua Type. Note that the type name
   --  is saved as a user data value in order to be able to perform type check
   --  when reimporting to ada.

   function To_Ada (State : Lua_State; Index : Lua_Index) return Ada_Type;
   --  Get an Ada_Type value from the stack. In case of type error
   --  Lua_Type_Error is raised

end Lua.Ada_Types;
