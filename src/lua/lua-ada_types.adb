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

package body Lua.Ada_Types is

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State; Index : Lua_Index)
      return Ada_Type
   is

      User_Data : constant Lua_User_Data := To_Ada (State, Index);

   begin
      Get_User_Value (State, Index);
      if Get_Type (State, -1) /= LUA_TTABLE then
         raise Lua_Type_Error with "not an ada type";
      end if;

      Get_Field (State, -1, "ada_type");

      declare
         Ada_Type_Name : constant String := To_Ada (State, -1);
      begin
         if Ada_Type_Name /= Name then
            raise Lua_Type_Error
              with "expect type " & Name & " but found " & Ada_Type_Name;
         end if;
      end;

      Pop (State, 2);

      declare
         Result : Ada_Type;
         for Result'Address use System.Address (User_Data);
         pragma Import (C, Result);
      begin
         return Result;
      end;
   end To_Ada;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Ada_Type)
   is
      Result_Addr : constant System.Address :=
                      New_User_Data (State, Data'Size);
      Result      : Ada_Type;
      pragma Import (C, Result);
      for Result'Address use Result_Addr;
   begin
      Result := Data;
      Create_Table (State);
      Push (State, Name);
      Set_Field (State, -2, "ada_type");
      Set_User_Value (State, -2);
   end Push;

end Lua.Ada_Types;
