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
      if B.Pos.Pos > B.S'Last then
         return ASCII.EOT;
      end if;

      return B.S (B.Pos.Pos);
   end Current;

   -------------
   -- Current --
   -------------

   function Current (B : Buffer; Size : Positive) return String is
   begin
      if B.Pos.Pos > B.S'Last then
         return "";
      end if;

      if B.Pos.Pos + Size - 1 > B.S'Last then
         return B.S (B.Pos.Pos .. B.S'Last);
      end if;

      return B.S (B.Pos.Pos .. B.Pos.Pos + Size - 1);
   end Current;

   --------------------
   -- Current_Lineno --
   --------------------

   function Current_Lineno (B : Buffer) return Natural is
   begin
      return B.Pos.Line;
   end Current_Lineno;

   -----------------
   -- Current_Pos --
   -----------------

   function Current_Pos (B : Buffer) return Text_Position is
   begin
      return B.Pos;
   end Current_Pos;

   -----------------
   -- Current_Pos --
   -----------------

   function Current_Pos (B : Buffer) return Natural is
   begin
      return B.Pos.Pos;
   end Current_Pos;

   -------------
   -- Forward --
   -------------

   procedure Forward (B : in out Buffer; Step : Positive := 1) is
   begin
      for J in 1 .. Step loop

         if B.Pos.Pos < B.S'Last and then B.S (B.Pos.Pos) = ASCII.LF then
            B.Pos.Line := B.Pos.Line + 1;
         end if;

         B.Pos.Pos := B.Pos.Pos + 1;
      end loop;

   end Forward;

   ----------------
   -- Get_Lineno --
   ----------------

   function Get_Lineno (T : Text_Position) return Natural is
   begin
      return T.Line;
   end Get_Lineno;

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos (T : Text_Position) return Natural is
   begin
      return T.Pos;
   end Get_Pos;

   -----------
   -- Image --
   -----------

   function Image (T : Text_Position) return String is
   begin
      return To_String (T.Line);
   end Image;

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
   -- Rewind --
   ------------

   procedure Rewind (B : in out Buffer; Step : Positive := 1) is
   begin
      if B.Pos.Pos - Step < 1 then
         B.Pos := (1, 1);
         return;
      end if;

      for J in 1 .. Step loop
         if B.Pos.Pos - J <=  B.S'Last and then
           B.S (B.Pos.Pos - J) = ASCII.LF
         then
            B.Pos.Line := B.Pos.Line - 1;
         end if;
      end loop;
      B.Pos.Pos := B.Pos.Pos - Step;
   end Rewind;

   -------------
   -- Set_Pos --
   -------------

   procedure Set_Pos (B : in out Buffer; P : Text_Position) is
   begin
      B.Pos := P;
   end Set_Pos;

   -----------
   -- Slice --
   -----------

   function Slice (B : Buffer; First, Last : Text_Position) return String is
   begin
      return B.S (First.Pos .. Last.Pos);
   end Slice;

   --------------
   -- Prev_Pos --
   --------------

   function Prev_Pos (B : Buffer) return Text_Position is
      Buffer_Copy : Buffer := B;
   begin
      Rewind (Buffer_Copy);
      return Current_Pos (Buffer_Copy);
   end Prev_Pos;
end Posix_Shell.Buffers;
