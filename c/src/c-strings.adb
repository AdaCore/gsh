------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2019, AdaCore                   --
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

with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

package body C.Strings is

   use type GS.String_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Indices, Indices_Access);

   procedure Allocate (Self    : in out CList;
                       Indexes : Natural;
                       Chars   : Natural);

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Self    : in out CList;
                       Indexes : Natural;
                       Chars   : Natural)
   is
      use type System.Address;
      use System.Storage_Elements;

      Idx_Max : Natural := (if Self.Idx = null then 8 else Self.Idx'Length);
      Str_Max : Natural := (if Self.Str = null then 128 else Self.Str'Length);
      Offset : constant System.Address :=
         (if Self.Str = null then System.Null_Address else
          Self.Str (1)'Address);
      Str_Reallocated : Boolean := False;

   begin
      --  We always need to ensure that we have at least one index more
      --  allocated
      while Self.Idx_Last + Indexes + 1 > Idx_Max loop
         Idx_Max := Idx_Max * 2;
      end loop;

      --  Likewise we ensure that the end of the table always contains
      --  a double null character. This will ease export to C structures
      --  use for example to represent environment.
      while Self.Str_Last + Chars + 2 > Str_Max loop
         Str_Max := Str_Max * 2;
      end loop;

      --  Perform reallocations
      if Self.Str = null or else Str_Max > Self.Str'Length then
         declare
            New_Str : constant GS.String_Access := new String (1 .. Str_Max);
         begin
            if Self.Str /= null then
               --  Copy previous content if necessary
               New_Str (1 .. Self.Str_Last) := Self.Str (1 .. Self.Str_Last);
               GS.Free (Self.Str);
            end if;
            New_Str (Self.Str_Last + 1) := ASCII.NUL;
            New_Str (Self.Str_Last + 2) := ASCII.NUL;
            Self.Str := New_Str;
            Str_Reallocated := True;
         end;
      end if;

      if Self.Idx = null or else Idx_Max > Self.Idx'Length or else
         Str_Reallocated
      then
         declare
            New_Idx : constant Indices_Access := new Indices (1 .. Idx_Max);
         begin
            for Index in 1 .. Self.Idx_Last loop
               New_Idx (Index) := Self.Str (1)'Address +
                  (Self.Idx (Index) - Offset);
            end loop;

            if Self.Idx /= null then
               Free (Self.Idx);
            end if;

            --  Self.Indexes null implies Last_Index set to 0
            New_Idx (Self.Idx_Last + 1) := System.Null_Address;
            Self.Idx := New_Idx;
         end;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out CList; Item : String) is
   begin
      Allocate (Self, 1, Item'Length + 1);

      Self.Idx_Last := Self.Idx_Last + 1;
      Self.Idx (Self.Idx_Last) := Self.Str (Self.Str_Last + 1)'Address;
      Self.Idx (Self.Idx_Last + 1) := System.Null_Address;

      Self.Str (Self.Str_Last + 1 .. Self.Str_Last + Item'Length) := Item;
      Self.Str (Self.Str_Last + Item'Length + 1) := ASCII.NUL;
      Self.Str (Self.Str_Last + Item'Length + 2) := ASCII.NUL;
      Self.Str (Self.Str_Last + Item'Length + 3) := ASCII.NUL;
      Self.Str_Last := Self.Str_Last + Item'Length + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out CList; Item : GS.String_List)
   is
   begin
      Allocate (Self, Item'Length, 0);
      for J in Item'Range loop
         Append (Self, Item (J).all);
      end loop;
   end Append;

   -----------------------
   -- As_C_String_Array --
   -----------------------

   function As_C_String_Array (Self : CList) return System.Address
   is
   begin
      if Self.Idx /= null then
         return Self.Idx (1)'Address;
      else
         return System.Null_Address;
      end if;
   end As_C_String_Array;

   -----------------------
   -- As_C_String_Block --
   -----------------------

   function As_C_String_Block (Self : CList) return System.Address
   is
   begin
      if Self.Str /= null then
         return Self.Str (1)'Address;
      else
         return System.Null_Address;
      end if;
   end As_C_String_Block;

   -------------
   -- As_List --
   -------------

   function As_List (Self : CList) return GS.String_List
   is
      Result : GS.String_List (1 .. Self.Idx_Last);
   begin
      for Index in 1 .. Self.Idx_Last loop
         Result (Index) := new String'(Element (Self, Index));
      end loop;
      return Result;
   end As_List;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Self : in out CList)
   is
   begin
      if Self.Str /= null then
         GS.Free (Self.Str);
      end if;
      if Self.Idx /= null then
         Free (Self.Idx);
      end if;
      Self.Idx_Last := 0;
      Self.Str_Last  := 0;
   end Deallocate;

   -------------
   -- Element --
   -------------

   function Element (Self : CList; Index : Positive) return String
   is
      use System.Storage_Elements;
   begin
      if Index < Self.Idx_Last then
         return Self.Str
           (Integer (Self.Idx (Index) - Self.Str (1)'Address) + 1 ..
            Integer (Self.Idx (Index + 1) - Self.Str (1)'Address) - 1);
      elsif Index = Self.Idx_Last then
         return Self.Str
           (Integer (Self.Idx (Index) - Self.Str (1)'Address) + 1 ..
            Self.Str_Last - 1);
      else
         raise Constraint_Error;
      end if;
   end Element;

   ----------
   -- Join --
   ----------

   function Join
     (Self      : CList;
      Separator : String;
      Quote     : String)
      return String
   is
      Str : String
        (1 .. Self.Str_Last - Self.Idx_Last +
         (Self.Idx_Last - 1) * Separator'Length
         + Self.Idx_Last * 2 * Quote'Length);
      Str_Idx : Integer := Str'First;

      SL : constant Natural := Separator'Length;
      QL : constant Natural := Quote'Length;
   begin
      if Self.Idx_Last = 0 then
         return "";
      else
         for Index in 1 .. Self.Idx_Last loop
            declare
               Item : constant String := Element (Self, Index);
               IL : constant Natural := Item'Length;
            begin
               if Index > 1 then
                  Str (Str_Idx .. Str_Idx + SL - 1) := Separator;
                  Str_Idx := Str_Idx + SL;
               end if;
               Str (Str_Idx .. Str_Idx + QL - 1) := Quote;
               Str_Idx := Str_Idx + QL;
               Str (Str_Idx .. Str_Idx + IL - 1) := Element (Self, Index);
               Str_Idx := Str_Idx + IL;
               Str (Str_Idx .. Str_Idx + QL - 1) := Quote;
               Str_Idx := Str_Idx + QL;
            end;
         end loop;

         return Str;
      end if;
   end Join;

   ------------
   -- Length --
   ------------

   function Length (Self : CList) return Natural
   is
   begin
      return Self.Idx_Last;
   end Length;

end C.Strings;
