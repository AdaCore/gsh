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

with Ada.Unchecked_Conversion;

package body Lua.Ada_Types is

   function Img2Addr is new Ada.Unchecked_Conversion
     (Image_Function, System.Address);
   function Addr2Img is new Ada.Unchecked_Conversion
     (System.Address, Image_Function);

   function Image_Lua_Wrapper (State : Lua_State) return Integer;
   pragma Convention (C, Image_Lua_Wrapper);
   --  Lua function registered as __tostring metamethod. The function is used
   --  to implement Register_Image_Function

   function Equal_To (State : Lua_State) return Integer;
   pragma Convention (C, Equal_To);
   --  Lua function used to implement the __eq metamethod

   procedure Create_Metatable (State : Lua_State);
   --  Initialize the metatable associated with Ada_Type object in Lua

   ----------------------
   -- Create_Metatable --
   ----------------------

   procedure Create_Metatable (State : Lua_State) is
   begin
      --  We assume that the metatable is the top of the stack at the beginning
      --  of the procedure.
      Push (State, "__eq");
      Push_Closure (State, Equal_To'Unrestricted_Access);
      Set_Table (State, -3);
   end Create_Metatable;

   --------------
   -- Equal_To --
   --------------

   function Equal_To (State : Lua_State) return Integer is
      Left  : constant Ada_Type := To_Ada (State, 1);
      Right : constant Ada_Type := To_Ada (State, 2);

   begin
      Push (State, Left = Right);
      return 1;
   end Equal_To;

   -----------------------
   -- Image_Lua_Wrapper --
   -----------------------

   function Image_Lua_Wrapper (State : Lua_State) return Integer is
      --  Get the argument
      Arg       : constant Ada_Type := To_Ada (State, 1);
      User_Data : constant Lua_User_Data := To_Ada (State, Upvalue_Index (1));
      Fun       : Image_Function;
   begin
      --  The Ada image function is an upvalue in the closure of our wrapper.
      Fun := Addr2Img (System.Address (User_Data));
      Push (State, Fun (Arg));
      return 1;
   end Image_Lua_Wrapper;

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
      Ada_Type_Exists : constant Boolean := New_Metatable (State, Name);
   begin
      if not Ada_Type_Exists then
         Create_Metatable (State);
      end if;
      Result := Data;
      Set_Metatable (State, -2);
   end Push;

   --------------------
   -- Register_Image --
   --------------------

   procedure Register_Image (State : Lua_State; Fun : Image_Function) is
      Ada_Type_Exists : constant Boolean := New_Metatable (State, Name);
   begin
      if not Ada_Type_Exists then
         Create_Metatable (State);
      end if;
      Push (State, "__tostring");
      Push (State, Lua_Light_User_Data (Img2Addr (Fun)));
      Push_Closure (State, Image_Lua_Wrapper'Unrestricted_Access, 1);
      Set_Table (State, -3);
      Pop (State);
   end Register_Image;

   ---------------------
   -- Register_Object --
   ---------------------

   procedure Register_Object
     (State : Lua_State; Name : String; Obj : Ada_Type)
   is
   begin
      Push (State, Obj);
      Register_Object (State, Name);
   end Register_Object;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State; Index : Lua_Index)
      return Ada_Type
   is
      User_Data : constant Lua_User_Data :=
        Test_User_Data (State, Index, Name);

   begin
      if User_Data = Lua_User_Data (System.Null_Address) then
         raise Lua_Type_Error
           with "expect type " & Name;
      end if;

      declare
         Result : Ada_Type;
         for Result'Address use System.Address (User_Data);
         pragma Import (C, Result);
      begin
         return Result;
      end;
   end To_Ada;

end Lua.Ada_Types;
