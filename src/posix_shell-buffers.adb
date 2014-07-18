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

with Posix_Shell.String_Utils; use Posix_Shell.String_Utils;

package body Posix_Shell.Buffers is

   -------------
   -- Current --
   -------------

   function Current (B : Buffer) return Character is
   begin
      if B.Pos.Offset > B.S'Last then
         return ASCII.EOT;
      end if;

      return B.S (B.Pos.Offset);
   end Current;

   -------------
   -- Current --
   -------------

   function Current (B : Buffer; Size : Positive) return String is
   begin
      if B.Pos.Offset > B.S'Last then
         return "";
      end if;

      if B.Pos.Offset + Size - 1 > B.S'Last then
         return B.S (B.Pos.Offset .. B.S'Last);
      end if;

      return B.S (B.Pos.Offset .. B.Pos.Offset + Size - 1);
   end Current;

   -------------
   -- Current --
   -------------

   function Current (B : Buffer) return Text_Position is
   begin
      return B.Pos;
   end Current;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line (B : Buffer) return Natural is
   begin
      return B.Pos.Line;
   end Current_Line;

   --------------------
   -- Current_Offset --
   --------------------

   function Current_Offset (B : Buffer) return Natural is
   begin
      return B.Pos.Offset;
   end Current_Offset;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (B : in out Buffer) is
   begin
      if B.S /= null then
         Free (B.S);
         B.S := null;
      end if;
   end Deallocate;

   -------------
   -- Forward --
   -------------

   procedure Forward (B : in out Buffer; Step : Positive := 1) is
   begin
      for J in 1 .. Step loop

         if B.Pos.Offset < B.S'Last and then B.S (B.Pos.Offset) = ASCII.LF then
            B.Pos.Line := B.Pos.Line + 1;
         end if;

         B.Pos.Offset := B.Pos.Offset + 1;
      end loop;

   end Forward;

   -----------
   -- Image --
   -----------

   function Image (T : Text_Position; Msg : String := "") return String is
   begin
      if Msg'Length = 0 then
         return To_String (T.Line);
      else
         return To_String (T.Line) & ":" & Msg;
      end if;
   end Image;

   ----------
   -- Line --
   ----------

   function Line (T : Text_Position) return Natural is
   begin
      return T.Line;
   end Line;

   function Line (T : Text_Position) return String is
   begin
      return To_String (T.Line);
   end Line;

   ----------------
   -- New_Buffer --
   ----------------

   function New_Buffer (Str : String) return Buffer is
      Result     : Buffer;
      Buffer_Str : constant String := Strip_CR (Str);

   begin
      Result.S := new String'(Buffer_Str);
      Result.Pos := (1, 1);
      return Result;
   end New_Buffer;

   --------------------------
   -- New_Buffer_From_File --
   --------------------------

   function New_Buffer_From_File (Filename : String) return Buffer is
      F : constant File_Descriptor := Open_Read (Filename, Binary);
      Expected_Bytes_Read : Integer;
      Bytes_Read : Integer;
   begin
      if F = Invalid_FD then
         --  The Open_Read failed.
         --  Error (Filename & ": Unable to open for reading");
         raise Buffer_Read_Error;
      end if;

      Expected_Bytes_Read := Integer (File_Length (F));

      declare
         Buffer_Str : aliased String (1 .. Expected_Bytes_Read);
      begin
         Bytes_Read := Read (F, Buffer_Str'Address, Expected_Bytes_Read);
         Close (F);

         if Bytes_Read /= Expected_Bytes_Read then
            raise Buffer_Read_Error;
         end if;

         return New_Buffer (Buffer_Str);
      end;
   end New_Buffer_From_File;

   ------------
   -- Offset --
   ------------

   function Offset (T : Text_Position) return Natural is
   begin
      return T.Offset;
   end Offset;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (B : in out Buffer; Step : Positive := 1) is
   begin
      if B.Pos.Offset - Step < 1 then
         B.Pos := (1, 1);
         return;
      end if;

      for J in 1 .. Step loop
         if B.Pos.Offset - J <=  B.S'Last and then
           B.S (B.Pos.Offset - J) = ASCII.LF
         then
            B.Pos.Line := B.Pos.Line - 1;
         end if;
      end loop;
      B.Pos.Offset := B.Pos.Offset - Step;
   end Rewind;

   ----------
   -- Seek --
   ----------

   procedure Seek (B : in out Buffer; T : Text_Position) is
   begin
      B.Pos := T;
   end Seek;

   -----------
   -- Slice --
   -----------

   function Slice (B : Buffer; First, Last : Text_Position) return String is
   begin
      return B.S (First.Offset .. Last.Offset);
   end Slice;

   --------------
   -- Prev_Pos --
   --------------

   function Previous (B : Buffer) return Text_Position is
      Buffer_Copy : Buffer := B;
   begin
      Rewind (Buffer_Copy);
      return Current (Buffer_Copy);
   end Previous;
end Posix_Shell.Buffers;
