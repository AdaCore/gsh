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

package body Lua.Utils is

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function (State : Lua_State; Name : String) return String
   is
   begin
      Get_Global (State, Name);
      PCall (State, 0, 1);

      declare
         Result : constant String := To_Ada (State, -1);
      begin
         Pop (State);
         return Result;
      end;
   end Call_Function;

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function
     (State : Lua_State;
      Name  : String;
      Arg   : String)
      return String
   is
   begin

      Get_Global (State, Name);
      Push (State, Arg);
      PCall (State, 1, 1);

      declare
         Result : constant String := To_Ada (State, -1);
      begin
         Pop (State);
         return Result;
      end;
   end Call_Function;

   function Lua_String_Filter (State : Lua_State) return Integer is
      Index : Lua_Index := Get_Top (State);
   begin
      declare
         Arg : constant String := To_Ada (State, 1);
         Result : constant String := Action (Arg);
      begin

         Push (State, Result);
      end;
      return 1;
   end Lua_String_Filter;
end Lua.Utils;
