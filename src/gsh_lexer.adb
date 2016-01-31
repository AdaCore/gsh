------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                   GSH                                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with Posix_Shell.Lexer; use Posix_Shell.Lexer;
with Ada.Command_Line; use Ada.Command_Line;
with Posix_Shell; use Posix_Shell;

---------
-- GSH --
---------

function GSH_Lexer return Integer is
   Status        : constant Integer := 0;
   Script_Buffer : Token_Buffer := New_Buffer_From_File (Argument (1));
   T             : Token;

begin
   Debug_Lexer := True;
   loop
      T := Read_Token (Script_Buffer);
      exit when Get_Token_Type (T) = T_EOF;
   end loop;
   return Status;
end GSH_Lexer;
