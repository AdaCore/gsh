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

with System.Address_To_Access_Conversions;

package body Lua.Ada_Limited_Types is

   package Conv is new System.Address_To_Access_Conversions (Ada_Type);

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance (State : Lua_State) return Integer is
      Arg_Num : constant Lua_Index := Get_Top (State);
      Result  : constant access Ada_Type := new Ada_Type;
   begin
      if Arg_Num /= 0 then
         Push (State, "no argument expected");
         return Error (State);
      end if;

      Push (State, Result);
      return 1;
   end New_Instance;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : access Ada_Type) is
   begin
      Push (State, Lua_Light_User_Data (Data.all'Address));
   end Push;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State  : Lua_State;
      Index  : Lua_Index)
      return access Ada_Type
   is
      User_Data : constant Lua_User_Data := To_Ada (State, Index);
      Result    : constant access Ada_Type :=
                    Conv.To_Pointer (System.Address (User_Data));

   begin
      return Result;
   end To_Ada;

end Lua.Ada_Limited_Types;
