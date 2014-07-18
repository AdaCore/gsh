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
   --  Represent a position in a buffer.

   Null_Text_Position : constant Text_Position;
   --  An invalid text position

   function Offset (T : Text_Position) return Natural;
   --  Return the offset corresponding to T

   function Line (T : Text_Position) return Natural;
   --  Return the line corresponding to position T

   function Line (T : Text_Position) return String;
   --  Return the line corresponding to position T as a string

   function Image (T : Text_Position; Msg : String := "") return String;
   --  Return a string image of position T. If Msg is not the null string then
   --  the image will contain the image of the T followed by a colon and the
   --  message.

   procedure Rewind  (B : in out Buffer; Step : Positive := 1);
   --  Rewind buffer B by Step positions

   procedure Forward (B : in out Buffer; Step : Positive := 1);
   --  Forward buffer B by Step positions

   procedure Seek (B : in out Buffer; T : Text_Position);
   --  Reset position of B to T

   function Current (B : Buffer) return Character;
   --  Get current character at current position

   function Current (B : Buffer; Size : Positive) return String;
   --  Get String of length Size starting at current position

   function Current (B : Buffer) return Text_Position;
   --  Get current position

   function Previous (B : Buffer) return Text_Position;
   --  Get position previous to the current one (i.e one character before)

   function Current_Offset (B : Buffer) return Natural;
   --  Get current offset

   function Current_Line (B : Buffer) return Natural;
   --  Get current line

   function New_Buffer (Str : String) return Buffer;
   --  Create a buffer from Str and set its position to the first character

   function New_Buffer_From_File (Filename : String) return Buffer;
   --  Create a buffer with the content of the file Filename and set its
   --  position to the first character in the buffer

   function Slice (B : Buffer; First, Last : Text_Position) return String;
   --  Return the string delimited by First and Last

   procedure Deallocate (B : in out Buffer);

private

   pragma Inline (Forward);
   pragma Inline (Rewind);
   pragma Inline (Offset);
   pragma Inline (Line);

   type Text_Position is record
      Offset : Natural;
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
