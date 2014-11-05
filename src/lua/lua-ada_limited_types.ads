------------------------------------------------------------------------------
--                              Ada Lua Binding                             --
--                                                                          --
--                         Copyright (C) 2014, AdaCore                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  Generic package that ease export and import to Lua of Ada limited types.
--  Note that because of limitation of limited types, the To_Ada function
--  returns an access to the value instead of the value itself.
--
--  One approach to import a value from Lua is to do the following
--
--  declare
--    Var : Ada_Type renames To_Ada (State, -1);
--  begin
--
--  Note also that internally we are using light user data. In that case no
--  user data can be associated with the Lua object, so we cannot perform any
--  type checks when importing from Lua.

generic
   type Ada_Type is limited private;
package Lua.Ada_Limited_Types is

   procedure Push (State : Lua_State; Data : access Ada_Type);
   --  Push an Ada_Type value as a LightUserData Lua Type.

   function To_Ada
     (State  : Lua_State;
      Index  : Lua_Index)
      return access Ada_Type;
   --  Get an Ada_Type value from the stack at position Index. No type check
   --  is performed.

   function New_Instance (State : Lua_State) return Integer;
   --  Function that can be registered in Lua to create new instance of
   --  Ada_Type
   pragma Convention (C, New_Instance);

end Lua.Ada_Limited_Types;
