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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Posix_Shell.String_Utils is

   ----------------------------
   -- Is_Valid_Variable_Name --
   ----------------------------

   function Is_Valid_Variable_Name (Name : String) return Boolean is
   begin
      --  For the definition of valid names, see: the Base Definitions
      --  volume of IEEE Std 1003.1-2001, Section 3.230, Name.
      --  ??? brobecker/2007-04-28: Double-check the implementation below
      --  ??? against the document above. I haven't been able to do so
      --  ??? at the time I implemented this, because I don't have access
      --  ??? to internet right now.

      --  Name must be at least one character long

      if Name'Length = 0 then
         return False;
      end if;

      --  The first character must be alphabetic

      if not Is_Letter (Name (Name'First)) and Name (Name'First) /= '_' then
         return False;
      end if;

      --  The remaining characters must be either alpha-numeric
      --  or an underscore.

      for J in Name'First + 1 .. Name'Last loop
         if not Is_Alphanumeric (Name (J)) and then Name (J) /= '_' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Variable_Name;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (S : String; Sub : String) return Boolean is
   begin
      if S'Length >= Sub'Length and then
        S (S'First .. S'First + Sub'Length - 1) = Sub
      then
         return True;
      else
         return False;
      end if;
   end Starts_With;

   ---------------
   -- Last_Line --
   ---------------

   function Last_Lines
     (S : String; Size : Natural) return String
   is
      Pos : Integer := S'Last;
      Newline_Found : Integer := 0;
   begin
      if S = "" then
         return "";
      end if;

      --  The last line might end or not with a LF character
      if S (Pos) = ASCII.LF then
         Pos := Pos - 1;
      end if;

      while Newline_Found < Size and then Pos >= S'First loop
         if S (Pos) = ASCII.LF then
            Newline_Found := Newline_Found + 1;
         end if;
         Pos := Pos - 1;
      end loop;

      --  Ignore last LF found
      if Newline_Found = Size then
         Pos := Pos + 1;
      end if;

      return S (Pos + 1 .. S'Last);
   end Last_Lines;

   ---------------
   -- From_Line --
   ---------------

   function From_Line (S : String; LineNo : Natural) return String
   is
      Pos          : Integer := S'First;
      Current_Line : Integer := 1;
   begin
      if S = "" then
         return "";
      end if;

      while Current_Line < LineNo loop
         pragma Loop_Invariant (Pos in S'Range);
         if S (Pos) = ASCII.LF then
            Current_Line := Current_Line + 1;
         end if;

         if Pos = S'Last then
            return "";
         end if;

         Pos := Pos + 1;
      end loop;

      return S (Pos .. S'Last);
   end From_Line;

   ----------------
   -- Is_Natural --
   ----------------

   function Is_Natural (S : String) return Boolean is
   begin
      if S = "" then
         return False;
      end if;

      for J in S'Range loop
         if S (J) not in '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Natural;

   -------------
   -- To_Line --
   -------------

   function To_Line (S : String; LineNo : Natural) return String
   is
      Pos           : Integer := S'First;
      Newline_Found : Integer := 0;
   begin
      if S = "" then
         return "";
      end if;

      while Newline_Found < LineNo loop
         pragma Loop_Invariant (Pos in S'Range);
         if S (Pos) = ASCII.LF then
            Newline_Found := Newline_Found + 1;
         end if;

         if Pos = S'Last then
            return S;
         end if;

         Pos := Pos + 1;
      end loop;

      return S (S'First .. Pos - 1);
   end To_Line;

   ---------------
   -- To_String --
   ---------------

   function To_String (I : Integer) return String is
      Str : constant String := Integer'Image (I);
   begin
      if I < 0 then
         return Str;
      else
         return Str (Str'First + 1 .. Str'Last);
      end if;
   end To_String;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Str : String) return String is
      Result : String (1 .. Str'Length);
      Last : Integer := 0;
   begin
      for J in Str'Range loop
         case Str (J) is
            when ASCII.CR =>
               if J /= Str'Last and then Str (J + 1) /= ASCII.LF then
                  Last := Last + 1;
                  Result (Last) := Str (J);
               end if;
            when others   =>
               Last := Last + 1;
               Result (Last) := Str (J);
         end case;
      end loop;
      if Last = 0 then
         return "";
      else
         return Result (1 .. Last);
      end if;
   end Strip_CR;

end Posix_Shell.String_Utils;
