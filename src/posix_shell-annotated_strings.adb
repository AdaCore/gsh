------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Annotated_Strings                      --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

package body Posix_Shell.Annotated_Strings is

   procedure Free (X : in out Str_Elements_Access);
   procedure Realloc_For_Chunk
     (Source      : in out Annotated_String;
      Chunk_Size  : Natural;
      Is_Slice    : Boolean := False);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Annotated_String) is
   begin
      if Object.Buffer /= Null_Elements'Access then
         Object.Ref_Counter.all := Object.Ref_Counter.all + 1;
      end if;
   end Adjust;

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

   procedure Finalize (Object : in out Annotated_String) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Str_Elements, Str_Elements_Access);

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Natural, Natural_Access);

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Boolean, Boolean_Access);

   begin
      if Object.Buffer /= Null_Elements'Access then
         if Object.Ref_Counter.all = 1 then
            Deallocate (Object.Buffer);
            Deallocate (Object.Ref_Counter);
            Deallocate (Object.Modified);
            Object.Buffer := Null_Annotated_String.Buffer;
            Object.Last := 0;
         else
            Object.Ref_Counter.all := Object.Ref_Counter.all - 1;
            if Object.Modified.all and then Object.Is_Modifier then
               Object.Modified.all := False;
            end if;
         end if;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Str_Elements_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Str_Elements, Str_Elements_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Annotated_String.Buffer then
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Annotated_String) is
   begin
      Object.Buffer := Null_Elements'Access;
      Object.Last := 0;
      Object.Ref_Counter := null;
      Object.Modified := null;
      Object.Is_Modifier := False;
   end Initialize;

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
      Chunk_Size  : Natural;
      Is_Slice    : Boolean := False)
   is
      Growth_Factor : constant := 32;
      --  The growth factor controls how much extra space is allocated when
      --  we have to increase the size of an allocated unbounded string. By
      --  allocating extra space, we avoid the need to reallocate on every
      --  append, particularly important when a string is built up by repeated
      --  append operations of small pieces. This is expressed as a factor so
      --  32 means add 1/32 of the length of the string as growth space.

      Min_Mul_Alloc : constant := Standard'Maximum_Alignment;
      --  Allocation will be done by a multiple of Min_Mul_Alloc This causes
      --  no memory loss as most (all?) malloc implementations are obliged to
      --  align the returned memory on the maximum alignment as malloc does not
      --  know the target alignment.

      S_Length : constant Natural := Source.Buffer'Length;

   begin
      --  Ada.Text_IO.Put_Line
      --   (Chunk_Size'Img & "," & S_Length'Img & "," & Source.last'Img);
      if Chunk_Size > S_Length - Source.Last
        or else (Source.Modified.all and then not Source.Is_Modifier)
        or else (Is_Slice and then Source.Ref_Counter.all > 1)
      then
         declare
            New_Size : constant Positive :=
              S_Length + Chunk_Size + (S_Length / Growth_Factor);

            New_Rounded_Up_Size : constant Positive :=
              ((New_Size - 1) / Min_Mul_Alloc + 1) *
                Min_Mul_Alloc;

            Tmp_Buffer : constant Str_Elements_Access :=
              new Str_Elements (1 .. New_Rounded_Up_Size);

         begin
            --  Create new string and notes
            Tmp_Buffer (1 .. Source.Last) := Source.Buffer (1 .. Source.Last);

            --  If not Null and reference counter to 1 then free
            if Source.Buffer /= Null_Elements'Access then
               if Source.Ref_Counter.all = 1 then
                  Free (Source.Buffer);
                  Source.Modified.all := False;
                  Source.Is_Modifier := False;
               else
                  Source.Ref_Counter.all := Source.Ref_Counter.all - 1;
                  Source.Ref_Counter := new Natural'(1);
                  Source.Modified := new Boolean'(False);
                  Source.Is_Modifier := False;
               end if;
            else
               Source.Ref_Counter := new Natural'(1);
               Source.Modified := new Boolean'(False);
               Source.Is_Modifier := False;
            end if;

            Source.Modified.all := False;
            Source.Buffer := Tmp_Buffer;
         end;
      else
         Source.Is_Modifier := True;
         Source.Modified.all := True;
      end if;
   end Realloc_For_Chunk;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Annotated_String; First, Last : Natural)
      return Annotated_String
   is
      Result        : Annotated_String := Null_Annotated_String;
      Result_Length : constant Natural := Last - First + 1;
   begin
      if Result_Length = 0 then
         return Result;

      else
         Realloc_For_Chunk (Result, Result_Length, True);
         Result.Buffer (1 .. Result_Length) := Source.Buffer (First .. Last);
         Result.Last := Result_Length;

         return Result;
      end if;
   end Slice;

   ---------
   -- Str --
   ---------

   function Str (Source : Annotated_String) return String is
      Result      : String (1 .. Source.Last);
      Result_Last : Integer := 0;
   begin
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

end Posix_Shell.Annotated_Strings;
