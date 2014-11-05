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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Lua is

   -----------------
   -- Check_Stack --
   -----------------

   function Check_Stack (State : Lua_State; N : Integer) return Boolean
   is
      function Internal (State : Lua_State; N : Integer) return Integer;
      pragma Import (C, Internal, "lua_checkstack");
   begin
      if Internal (State, N) = 0 then
         return False;
      else
         return True;
      end if;
   end Check_Stack;

   -------------
   -- Compare --
   -------------

   function Compare
     (State    : Lua_State;
      Index1   : Lua_Index;
      Index2   : Lua_Index;
      Operator : Lua_Compare_Op)
      return Boolean
   is
      function Internal
        (State    : Lua_State;
         Index1   : Lua_Index;
         Index2   : Lua_Index;
         Operator : Lua_Compare_Op)
         return Integer;
      pragma Import (C, Internal, "lua_compare");
   begin
      if Internal (State, Index1, Index2, Operator) = 0 then
         return False;
      else
         return True;
      end if;
   end Compare;

   ---------------
   -- Get_Field --
   ---------------

   procedure Get_Field
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String)
   is

      procedure Internal
        (State : Lua_State;
         Index : Lua_Index;
         Name  : chars_ptr);
      pragma Import (C, Internal, "lua_getfield");

      Result : chars_ptr := New_String (Name);

   begin

      Internal (State, Index, Result);
      Free (Result);
   end Get_Field;

   ----------------
   -- Get_Global --
   ----------------

   procedure Get_Global
     (State : Lua_State;
      Name  : String)
   is
      procedure Internal
        (State : Lua_State;
         Name  : chars_ptr);
      pragma Import (C, Internal, "lua_getglobal");

      Name_Ptr : chars_ptr := New_String (Name);
   begin
      Internal (State, Name_Ptr);
      Free (Name_Ptr);
   end Get_Global;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function
     (State : Lua_State;
      Index : Lua_Index)
      return Boolean
   is

      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Integer;
      pragma Import (C, Internal, "lua_iscfunction");

      Result : constant Integer := Internal (State, Index);
   begin
      if Result = 1 then
         return True;
      else
         return False;
      end if;
   end Is_Function;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (State : Lua_State; Index : Lua_Index) return Boolean is

      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Integer;
      pragma Import (C, Internal, "lua_isnumber");

      Result : constant Integer := Internal (State, Index);
   begin
      if Result = 1 then
         return True;
      else
         return False;
      end if;
   end Is_Number;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (State : Lua_State; Index : Lua_Index) return Boolean is

      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Integer;
      pragma Import (C, Internal, "lua_isstring");

      Result : constant Integer := Internal (State, Index);
   begin
      if Result = 1 then
         return True;
      else
         return False;
      end if;
   end Is_String;

   ------------------
   -- Is_User_Data --
   ------------------

   function Is_User_Data
     (State : Lua_State;
      Index : Lua_Index)
      return Boolean
   is

      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Integer;
      pragma Import (C, Internal, "lua_isuserdata");

      Result : constant Integer := Internal (State, Index);
   begin
      if Result = 1 then
         return True;
      else
         return False;
      end if;
   end Is_User_Data;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (State    : Lua_State;
      Filename : String;
      Mode     : String := "")
      return Lua_Return_Code
   is
      function Internal
        (State    : Lua_State;
         Filename : chars_ptr;
         Mode     : chars_ptr)
         return Lua_Return_Code;
      pragma Import (C, Internal, "luaL_loadfilex");

      Filename_Ptr : chars_ptr := New_String (Filename);
      Mode_Ptr     : chars_ptr := Null_Ptr;
      Result       : Lua_Return_Code;
   begin
      if Mode /= "" then
         Mode_Ptr := New_String (Mode);
      end if;

      Result := Internal (State, Filename_Ptr, Mode_Ptr);

      if Mode_Ptr /= Null_Ptr then
         Free (Mode_Ptr);
      end if;

      Free (Filename_Ptr);

      return Result;
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (State    : Lua_State;
      Filename : String;
      Mode     : String := "")
   is
      Result : constant Lua_Return_Code := Load_File (State, Filename, Mode);

   begin
      if Result /= LUA_OK then
         declare
            Error_Msg : constant String := To_Ada (State, -1);
         begin
            Pop (State);
            raise Lua_Error with Result'Img & ": " & Error_Msg;
         end;
      end if;
   end Load_File;

   ----------
   -- Next --
   ----------

   function Next (State : Lua_State; Index : Lua_Index) return Boolean
   is
      function Internal (State : Lua_State; Index : Lua_Index) return Integer;
      pragma Import (C, Internal, "lua_next");

      Result : constant Integer := Internal (State, Index);
   begin
      if Result = 0 then
         return False;
      else
         return True;
      end if;
   end Next;

   -----------
   -- PCall --
   -----------

   procedure PCall
     (State    : Lua_State;
      Nargs    : Integer := 0;
      NResults : Integer := 0;
      Err_Fun  : Integer := 0;
      Context  : Integer := 0;
      Cont_Fun : Lua_Function := null)
   is
      Result : constant Lua_Return_Code := PCall
        (State, Nargs, NResults, Err_Fun, Context, Cont_Fun);

   begin
      if Result /= LUA_OK then
         declare
            Error_Msg : constant String := To_Ada (State, -1);
         begin
            Pop (State);
            raise Lua_Error with Result'Img & ": " & Error_Msg;
         end;
      end if;
   end PCall;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (State : Lua_State;
      N     : Integer := 1)
   is
   begin
      Set_Top (State, -N - 1);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State) is
      procedure Internal (State : Lua_State);
      pragma Import (C, Internal, "lua_pushnil");

   begin
      Internal (State);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Lua_Float) is

      procedure Internal (State : Lua_State; Data : Lua_Float);
      pragma Import (C, Internal, "lua_pushnumber");
   begin
      Internal (State, Data);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Lua_Integer) is

      procedure Internal (State : Lua_State; Data : Lua_Integer);
      pragma Import (C, Internal, "lua_pushinteger");
   begin
      Internal (State, Data);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Lua_Unsigned) is

      procedure Internal (State : Lua_State; Data : Lua_Unsigned);
      pragma Import (C, Internal, "lua_pushunsigned");
   begin
      Internal (State, Data);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Lua_Light_User_Data)
   is
      procedure Internal (State : Lua_State; Data : Lua_Light_User_Data);
      pragma Import (C, Internal, "lua_pushlightuserdata");
   begin
      Internal (State, Data);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : Boolean) is
      procedure Internal (State : Lua_State; Data : Integer);
      pragma Import (C, Internal, "lua_pushboolean");
   begin
      if Data then
         Internal (State, 1);
      else
         Internal (State, 0);
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (State : Lua_State; Data : String) is
      function Internal
        (State    : Lua_State;
         Str_Addr : System.Address;
         Str_Size : size_t)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "lua_pushlstring");

      Result : chars_ptr;
      pragma Unreferenced (Result);
   begin
      Result := Internal (State, Data'Address, Data'Length);
   end Push;

   -----------------------
   -- Register_Function --
   -----------------------

   procedure Register_Function
     (State : Lua_State;
      Name  : String;
      Fun   : Lua_Function)
   is
      Start                     : Integer := Name'First;
      Is_First                  : Boolean := True;
      Need_Global               : Boolean := False;
      Pop_Times                 : Integer := 0;
      Set_Table_Times           : Integer := 0;
      Global_First, Global_Last : Integer := 0;
   begin
      --  check that name does not start or ends with . and cannot be empty
      --  ???

      for Index in Name'Range loop
         if Name (Index) = '.' then

            declare
               Name_Comp : constant String := Name (Start .. Index - 1);
               Comp_Type : Lua_Type;
            begin

               if Is_First then
                  Get_Global (State, Name_Comp);
                  Comp_Type := Get_Type (State, -1);
                  Global_First := Start;
                  Global_Last := Index - 1;
               else
                  Get_Field (State, -1, Name_Comp);
                  Comp_Type := Get_Type (State, -1);
               end if;

               if Comp_Type = LUA_TNIL then
                  --  Create the table
                  Pop (State);
                  if not Is_First then
                     Push (State, Name_Comp);
                     Set_Table_Times := Set_Table_Times + 1;
                  else

                     Need_Global := True;
                  end if;

                  Create_Table (State);

               elsif Comp_Type /= LUA_TTABLE then
                  raise Lua_Type_Error with "expecting table";
               else
                  Pop_Times := Pop_Times + 1;
               end if;

               Is_First := False;
            end;
            Start := Index + 1;
         end if;
      end loop;

      if Start = Name'First then
         --  This means that the function should registered at toplevel
         Push_Closure (State, Fun);
         Set_Global (State, Name);

      else
         --  At least one dot has been found so create a hierarchy
         Set_Field (State, -1, Name (Start .. Name'Last),
                    Fun, Override => False);

         for J in 1 .. Set_Table_Times loop
            Set_Table (State, -3);
         end loop;

         if Pop_Times > 0 then
            Pop (State, Pop_Times);
         end if;

         if Need_Global then
            Set_Global (State, Name (Global_First .. Global_Last));
         end if;
      end if;
   end Register_Function;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String;
      Override : Boolean := True)
   is

      procedure Internal
        (State : Lua_State;
         Index : Lua_Index;
         Name  : chars_ptr);
      pragma Import (C, Internal, "lua_setfield");

      Result : chars_ptr := New_String (Name);
   begin
      if not Override then
         Get_Field (State, Index, Name);
         if Get_Type (State, -1) /= LUA_TNIL then
            Pop (State);
            Free (Result);
            raise Lua_Override_Error with "element already set";
         end if;
         Pop (State);
      end if;
      Internal (State, Index, Result);
      Free (Result);
   end Set_Field;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (State    : Lua_State;
      Index    : Lua_Index;
      Name     : String;
      Fun      : Lua_Function;
      Override : Boolean := True)
   is
      Table_Index : constant Lua_Index := Absolute_Index (State, Index);
   begin
      Push_Closure (State, Fun);
      begin
         Set_Field (State, Table_Index, Name, Override);
      exception
         when Lua_Override_Error =>
            Pop (State);
            raise;
      end;
   end Set_Field;

   ----------------
   -- Set_Global --
   ----------------

   procedure Set_Global
     (State : Lua_State;
      Name  : String)
   is

      procedure Internal
        (State : Lua_State;
         Name  : chars_ptr);
      pragma Import (C, Internal, "lua_setglobal");

      Name_Ptr : chars_ptr := New_String (Name);
   begin
      Internal (State, Name_Ptr);
      Free (Name_Ptr);
   end Set_Global;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_Function
   is

      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Lua_Function;
      pragma Import (C, Internal, "lua_tocfunction");
   begin
      return Internal (State, Index);
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_User_Data
   is
      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return System.Address;
      pragma Import (C, Internal, "lua_touserdata");

   begin
      return Lua_User_Data (Internal (State, Index));
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_State
   is
      function Internal
        (State : Lua_State; Index : Lua_Index) return Lua_State;
      pragma Import (C, Internal, "lua_tothread");
   begin
      return Internal (State, Index);
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State   : Lua_State;
      Index   : Lua_Index)
      return Lua_Float
   is
      function Internal
        (State   : Lua_State;
         Index   : Lua_Index;
         Success : in out Integer)
         return Lua_Float;
      pragma Import (C, Internal, "lua_tonumberx");

      Success : Integer := 0;
      Result  : constant Lua_Float := Internal (State, Index, Success);
   begin
      if Success = 0 then
         raise Lua_Type_Error with "an integer was expected";
      end if;

      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State   : Lua_State;
      Index   : Lua_Index)
      return Lua_Integer
   is
      function Internal
        (State   : Lua_State;
         Index   : Lua_Index;
         Success : in out Integer)
         return Lua_Integer;
      pragma Import (C, Internal, "lua_tointegerx");

      Success : Integer := 0;
      Result  : constant Lua_Integer := Internal (State, Index, Success);
   begin
      if Success = 0 then
         raise Lua_Type_Error with "an integer was expected";
      end if;

      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State   : Lua_State;
      Index   : Lua_Index)
      return Lua_Unsigned
   is
      function Internal
        (State   : Lua_State;
         Index   : Lua_Index;
         Success : in out Integer)
         return Lua_Unsigned;
      pragma Import (C, Internal, "lua_tounsignedx");

      Success : Integer := 0;
      Result  : constant Lua_Unsigned := Internal (State, Index, Success);
   begin
      if Success = 0 then
         raise Lua_Type_Error with "an integer was expected";
      end if;

      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Boolean
   is
      function Internal
        (State : Lua_State;
         Index : Lua_Index)
         return Integer;
      pragma Import (C, Internal, "lua_toboolean");
   begin
      if Internal (State, Index) = 0 then
         return False;
      else
         return True;
      end if;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return String
   is
      function Internal
        (State : Lua_State;
         Index  : Integer;
         Length : out size_t)
         return chars_ptr;
      pragma Import (C, Internal, "lua_tolstring");

      Length : size_t;
   begin
      return Value (Internal (State, Index, Length));
   end To_Ada;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (State : Lua_State;
      Index : Lua_Index)
      return String
   is

      function Internal
        (State : Lua_State; Index : Lua_Index)
         return chars_ptr;
      pragma Import (C, Internal, "lua_typename");

      Result : constant chars_ptr := Internal (State, Index);
   begin
      return Value (Result);
   end Type_Name;

end Lua;
