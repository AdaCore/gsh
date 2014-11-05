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

with Lua.Ada_Types;
with Lua.Ada_Limited_Types;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package body Posix_Shell.Lua_Bindings is

   package Lua_Token_Buffer is new Lua.Ada_Limited_Types
     (Token_Buffer);
   use Lua_Token_Buffer;

   package Lua_Token is new Lua.Ada_Types
     ("token", Token);
   use Lua_Token;

   function Lua_New_Buffer_From_File (State : Lua_State) return Integer;
   pragma Convention (C, Lua_New_Buffer_From_File);

   function Lua_Read_Token (State : Lua_State) return Integer;
   pragma Convention (C, Lua_Read_Token);

   function Lua_Image (State : Lua_State) return Integer;
   pragma Convention (C, Lua_Image);

   function Lua_New_Buffer_From_File (State : Lua_State) return Integer
   is
      Filename : constant String := To_Ada (State, 1);
      Result   : constant access Token_Buffer :=
        new Token_Buffer'(New_Buffer_From_File (Filename));
   begin
      Push (State, Result);
      return 1;
   end Lua_New_Buffer_From_File;

   function Lua_Read_Token (State : Lua_State) return Integer
   is
      Buffer : constant access Token_Buffer := To_Ada (State, 1);
      Result : constant Token := Read_Token (Buffer.all);
   begin
      Push (State, Result);
      return 1;
   end Lua_Read_Token;

   function Lua_Image (State : Lua_State) return Integer is
      T      : constant Token := To_Ada (State, 1);
      Result : constant String := Image (T);
   begin
      Push (State, Result);
      return 1;
   end Lua_Image;

   procedure Initialize (State : Lua_State) is
   begin
      Lua.Register_Function (State,
                             "Posix_Shell.Lexer.New_Buffer_From_File",
                             Lua_New_Buffer_From_File'Access);
      Lua.Register_Function (State,
                             "Posix_Shell.Lexer.Read_Token",
                             Lua_Read_Token'Access);
      Lua.Register_Function (State,
                             "Posix_Shell.Lexer.Image",
                             Lua_Image'Access);
   end Initialize;

end Posix_Shell.Lua_Bindings;
