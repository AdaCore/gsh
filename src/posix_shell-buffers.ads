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

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Posix_Shell.Buffers is

   type Buffer is private;
   Null_Buffer : constant Buffer;

   type Text_Position is private;
   Null_Text_Position : constant Text_Position;

   function Get_Pos (T : Text_Position) return Natural;
   pragma Inline (Get_Pos);

   function Get_Lineno (T : Text_Position) return Natural;
   function Image (T : Text_Position) return String;

   procedure Rewind  (B : in out Buffer; Step : Positive := 1);
   procedure Forward (B : in out Buffer; Step : Positive := 1);
   procedure Set_Pos (B : in out Buffer; P : Text_Position);

   function Current (B : Buffer) return Character;
   function Current (B : Buffer; Size : Positive) return String;

   function Current_Pos (B : Buffer) return Text_Position;
   function Prev_Pos    (B : Buffer) return Text_Position;
   function Current_Pos (B : Buffer) return Natural;
   function Current_Lineno (B : Buffer) return Natural;

   function New_Buffer (Str : String) return Buffer;
   function New_Buffer_From_File (Filename : String) return Buffer;
   function Slice (B : Buffer; First, Last : Text_Position) return String;
private
   pragma Inline (Forward);
   pragma Inline (Rewind);

   type Text_Position is record
      Pos    : Natural;
      Line   : Natural;
   end record;

   Null_Text_Position : constant Text_Position := (0, 0);

   type Buffer is record
      S   : String_Access;
      Pos : Text_Position;
   end record;

   Null_Buffer : constant Buffer :=
     (S   => null,
      Pos => Null_Text_Position);

end Posix_Shell.Buffers;
