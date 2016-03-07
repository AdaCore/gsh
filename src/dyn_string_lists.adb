------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

package body Dyn_String_Lists is

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Source : in out Dyn_String_List;
      Item   : String)
   is
      Result : Dyn_String_List := Null_String_List;
   begin
      Append (Result, Item);
      Append (Result, Source);
      Source := Result;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : String)
   is
      New_Max_Size : Natural := Source.Max_Size;
   begin

      while Source.Size + Item'Length + 1 > New_Max_Size loop
         if New_Max_Size = 0 then
            New_Max_Size := 128;
         else
            New_Max_Size := New_Max_Size * 2;
         end if;
      end loop;

      if New_Max_Size > Source.Max_Size then
         declare
            New_Content : constant String_Access :=
              new String (1 .. New_Max_Size);
         begin
            if Source.Size > 0 then
               New_Content (1 .. Source.Size) :=
                 Source.Content (1 .. Source.Size);
               Free (Source.Content);
            end if;
            Source.Content := New_Content;
            Source.Max_Size := New_Max_Size;
         end;
      end if;

      Source.Content (Source.Size + 1 .. Source.Size + Item'Length) := Item;
      Source.Content (Source.Size + Item'Length + 1) := ASCII.NUL;

      Source.Size := Source.Size + Item'Length + 1;
      Source.Length := Source.Length + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : String_List)
   is
   begin
      for J in Item'Range loop
         Append (Source, Item (J).all);
      end loop;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : Dyn_String_List)
   is
      New_Max_Size : Natural := Source.Max_Size;
   begin

      if Item.Size = 0 then
         return;
      end if;

      while Source.Size + Item.Size > New_Max_Size loop
         if New_Max_Size = 0 then
            New_Max_Size := 128;
         else
            New_Max_Size := New_Max_Size * 2;
         end if;
      end loop;

      if New_Max_Size > Source.Max_Size then
         declare
            New_Content : constant String_Access :=
              new String (1 .. New_Max_Size);
         begin
            if Source.Size > 0 then
               New_Content (1 .. Source.Size) :=
                 Source.Content (1 .. Source.Size);
               Free (Source.Content);
            end if;
            Source.Content := New_Content;
            Source.Max_Size := New_Max_Size;
         end;
      end if;

      Source.Content (Source.Size + 1 .. Source.Size + Item.Size) :=
        Item.Content (1 .. Item.Size);
      Source.Size := Source.Size + Item.Size;
      Source.Length := Source.Length + Item.Length;
   end Append;

   -------------
   -- Content --
   -------------

   function Content
     (Source : Dyn_String_List) return String_List
   is
      Result : String_List (1 .. Source.Length);
      Result_Index : Natural := 1;
      Word_Begin : Natural := 1;
   begin

      for J in 1 .. Source.Size loop
         if Source.Content (J) = ASCII.NUL then
            Result (Result_Index) :=
              new String'(Source.Content (Word_Begin .. J - 1));
            Word_Begin := J + 1;
            Result_Index := Result_Index + 1;
         end if;
      end loop;

      return Result;
   end Content;

   ------------
   -- Length --
   ------------

   function Length (Source : Dyn_String_List) return Integer
   is
   begin
      return Source.Length;
   end Length;

end Dyn_String_Lists;
