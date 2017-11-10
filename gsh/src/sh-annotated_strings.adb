------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Annotated_Strings                      --
--                                                                          --
--                                 B o d y                                  --
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

package body Sh.Annotated_Strings is

   Null_Element : aliased constant Str_Element := (Kind => E_NULL);
   Null_Elements : aliased Str_Elements :=
     (1 .. 0 => Null_Element);

   procedure Free (X : in out Str_Elements_Access);
   procedure Realloc_For_Chunk
     (Source      : in out Annotated_String;
      Chunk_Size  : Natural);

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String;
      C      : Character)
   is
   begin
      Realloc_For_Chunk (Source, 1);
      Source.Buffer (Source.Last + 1) := (Kind => E_CHAR, Char => C);
      Source.Last := Source.Last + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String;
      A      : Annotation)
   is
   begin
      Realloc_For_Chunk (Source, 1);
      Source.Buffer (Source.Last + 1) := (Kind => E_CTRL, Ctrl => A);
      Source.Last := Source.Last + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String; S : String)
   is
      function To_Elements (S : String) return Str_Elements;

      function To_Elements (S : String) return Str_Elements
      is
         Result : Str_Elements (1 .. S'Length) := (others => (E_CHAR, ' '));
      begin
         for Index in S'Range loop
            Result (Index - S'First + 1).Char := S (Index);
         end loop;
         return Result;
      end To_Elements;

   begin
      if S'Length > 0 then
         Realloc_For_Chunk (Source, S'Length);
         Source.Buffer (Source.Last + 1 .. Source.Last + S'Length) :=
           To_Elements (S);
         Source.Last := Source.Last + S'Length;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String; New_Item : Annotated_String)
   is
   begin
      if New_Item.Last > 0 then
         Realloc_For_Chunk (Source, New_Item.Last);
         Source.Buffer (Source.Last + 1 .. Source.Last + New_Item.Last) :=
           New_Item.Buffer (1 .. New_Item.Last);
         Source.Last := Source.Last + New_Item.Last;
      end if;
   end Append;

   --------------
   -- Finalize --
   --------------

   procedure Deallocate (Object : in out Annotated_String) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Str_Elements, Str_Elements_Access);
   begin
      if Object.Buffer /= null then
         Deallocate (Object.Buffer);
         Object.Buffer := null;
         Object.Last := 0;
      end if;
   end Deallocate;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Str_Elements_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Str_Elements, Str_Elements_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Elements'Access then
         Deallocate (X);
      end if;
   end Free;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Source : Annotated_String; Index : Positive)
      return Str_Element
   is
   begin
      return Source.Buffer (Index);
   end Get_Element;

   -----------
   -- Image --
   -----------

   function Image (Source : Annotated_String) return String is
      Result      : String (1 .. Source.Last * 5);
      Result_Last : Integer := 0;
   begin
      if Source.Buffer = null then
         return "";
      end if;

      for Index in 1 .. Source.Last loop
         declare
            F : constant Integer := Result_Last + 1;
            E : constant Str_Element := Source.Buffer (Index);
         begin
            case E.Kind is
               when E_CHAR =>
                  Result (F) := E.Char;
                  Result_Last := F;
               when E_CTRL =>
                  case E.Ctrl is
                     when NULL_STRING         =>
                        Result (F .. F + 3) := "<N/>";
                        Result_Last := F + 3;
                     when QUOTED_NULL_STRING  =>
                        Result (F .. F + 3) := "<Q/>";
                        Result_Last := F + 3;
                     when UNSPLITABLE_BEGIN =>
                        Result (F .. F + 2) := "<U>";
                        Result_Last := F + 2;
                     when UNSPLITABLE_END =>
                        Result (F .. F + 3) := "</U>";
                        Result_Last := F + 3;
                     when FIELD_SEP           =>
                        Result (F .. F + 3) := "<F/>";
                        Result_Last := F + 3;
                  end case;
               when others =>
                  null;
            end case;

         end;
      end loop;

      return Result (1 .. Result_Last);
   end Image;

   ------------
   -- Length --
   ------------

   function Length (Source : Annotated_String) return Natural
   is
   begin
      return Source.Last;
   end Length;

   -----------------------
   -- Realloc_For_Chunk --
   -----------------------

   procedure Realloc_For_Chunk
     (Source      : in out Annotated_String;
      Chunk_Size  : Natural)
   is
      Prev_Length : Natural;
      New_Length  : Natural;
   begin

      if Source.Buffer = null then
         New_Length := Standard'Maximum_Alignment;
         Prev_Length := 0;
      else
         Prev_Length := Source.Buffer'Length;
         New_Length := Prev_Length;
      end if;

      while Chunk_Size + Source.Last > New_Length loop
         New_Length := New_Length * 2;
      end loop;

      if New_Length <= Prev_Length then
         return;
      end if;

      if Prev_Length = 0 then
         Source.Buffer := new Str_Elements (1 .. New_Length);
      else
         declare
            New_Buffer : constant Str_Elements_Access :=
              new Str_Elements (1 .. New_Length);
         begin
            New_Buffer (1 .. Source.Last) := Source.Buffer (1 .. Source.Last);
            Free (Source.Buffer);
            Source.Buffer := New_Buffer;
         end;
      end if;

   end Realloc_For_Chunk;

   ---------
   -- Str --
   ---------

   function Str (Source : Annotated_String) return String is
      Result      : String (1 .. Source.Last);
      Result_Last : Integer := 0;
   begin
      if Source.Buffer = null then
         return "";
      end if;

      for Index in Source.Buffer'Range loop
         case Source.Buffer (Index).Kind is
            when E_CHAR =>
               Result_Last := Result_Last + 1;
               Result (Result_Last) := Source.Buffer (Index).Char;
            when E_CTRL =>
               if Source.Buffer (Index).Ctrl = FIELD_SEP then
                  Result_Last := Result_Last + 1;
                  Result (Result_Last) := ' ';
               end if;
            when others =>
               null;
         end case;
      end loop;
      return Result (1 .. Result_Last);
   end Str;

end Sh.Annotated_Strings;
