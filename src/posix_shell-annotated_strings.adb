with Ada.Unchecked_Deallocation;

package body Posix_Shell.Annotated_Strings is

   procedure Free (X : in out String_Access);
   procedure Free (X : in out Annotations_Access);
   procedure Realloc_For_Chunk
     (Source      : in out Annotated_String;
      Chunk_Size  : Natural;
      Is_Slice    : Boolean := False);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Annotated_String) is
   begin
      if Object.Str /= Null_Str'Access then
         Object.Ref_Counter.all := Object.Ref_Counter.all + 1;
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String;
      C : Character;
      A : Annotation)
   is
   begin
      Realloc_For_Chunk (Source, 1);
      Source.Str (Source.Last + 1) := C;
      Source.Notes (Source.Last + 1) := A;
      Source.Last := Source.Last + 1;
      if A = NULL_STRING then
         Source.Has_Null_String := True;
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
         Source.Str (Source.Last + 1 .. Source.Last + New_Item.Last) :=
           New_Item.Str (1 .. New_Item.Last);
         Source.Notes (Source.Last + 1 .. Source.Last + New_Item.Last) :=
           New_Item.Notes (1 .. New_Item.Last);
         Source.Last := Source.Last + New_Item.Last;
         Source.Has_Null_String :=
           Source.Has_Null_String or New_Item.Has_Null_String;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Annotated_String;
                     S : String;
                     A : Annotation)
   is
      Add_Length : constant Natural := S'Length;
   begin
      if Add_Length > 0 then
         Realloc_For_Chunk (Source, Add_Length);
         Source.Str (Source.Last + 1 .. Source.Last + Add_Length) := S;
         Source.Notes (Source.Last + 1 .. Source.Last + Add_Length) :=
           (1 .. Add_Length => A);
         Source.Last := Source.Last + Add_Length;
         Source.Has_Null_String := Source.Has_Null_String or (A = NULL_STRING);
      end if;
   end Append;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Annotated_String) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Annotations, Annotations_Access);

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Natural, Natural_Access);

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Boolean, Boolean_Access);

   begin
      if Object.Str /= Null_Str'Access then
         if Object.Ref_Counter.all = 1 then
            Deallocate (Object.Str);
            Deallocate (Object.Notes);
            Deallocate (Object.Ref_Counter);
            Deallocate (Object.Modified);
            Object.Str := Null_Annotated_String.Str;
            Object.Notes := Null_Annotated_String.Notes;
            Object.Last := 0;
            Object.Has_Null_String := False;
         else
            Object.Ref_Counter.all := Object.Ref_Counter.all - 1;
            if Object.Modified.all and then Object.I_Am_Modifier then
               Object.Modified.all := False;
            end if;
         end if;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out String_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (String, String_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Annotated_String.Str then
         Deallocate (X);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Annotations_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Annotations, Annotations_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Annotated_String.Notes then
         Deallocate (X);
      end if;
   end Free;

   --------------------
   -- Get_Annotation --
   --------------------

   function Get_Annotation
     (Source : Annotated_String; Index : Positive)
      return Annotation
   is
   begin
      return Source.Notes (Index);
   end Get_Annotation;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character
     (Source : Annotated_String; Index : Positive)
      return Character
   is
   begin
      return Source.Str (Index);
   end Get_Character;

   ---------------------
   -- Has_Null_String --
   ---------------------

   function Has_Null_String (Source : Annotated_String) return Boolean is
   begin
      return Source.Has_Null_String;
   end Has_Null_String;

   -----------
   -- Image --
   -----------

   function Image (Source : Annotated_String) return String is
      Result : String (1 .. Source.Last * 8);
      Result_Last : Integer := 0;
   begin

      for Index in 1 .. Source.Last loop
         declare
            F : constant Integer := Result_Last + 1;
         begin
            case Source.Notes (Index) is
               when NO_ANNOTATION =>
                  Result (F) := Source.Str (Index);
                  Result_Last := F;
               when SINGLE_QUOTE_BEGIN  =>
                  Result (F .. F + 3) := "<S>" & Source.Str (Index);
                  Result_Last := F + 3;
               when SINGLE_QUOTE_END    =>
                  Result (F .. F + 4) := Source.Str (Index) & "</S>";
                  Result_Last := F + 4;
               when DOUBLE_QUOTE_BEGIN  =>
                  Result (F .. F + 3) := "<D>" & Source.Str (Index);
                  Result_Last := F + 3;
               when DOUBLE_QUOTE_END    =>
                  Result (F .. F + 4) := Source.Str (Index) & "</D>";
                  Result_Last := F + 4;
               when ESCAPE_SEQUENCE     =>
                  Result (F .. F + 7) := "<E>" & Source.Str (Index) & "</E>";
                  Result_Last := F + 7;
               when COMMAND_SUBST_BEGIN =>
                  Result (F .. F + 3) := "<C>" & Source.Str (Index);
                  Result_Last := F + 3;
               when COMMAND_SUBST_END   =>
                  Result (F .. F + 4) := Source.Str (Index) & "</C>";
                  Result_Last := F + 4;
               when PARAM_EVAL_BEGIN    =>
                  Result (F .. F + 3) := "<R>" & Source.Str (Index);
                  Result_Last := F + 3;
               when PARAM_EVAL_END      =>
                  Result (F .. F + 4) := Source.Str (Index) & "</R>";
                  Result_Last := F + 4;
               when NULL_STRING         =>
                  Result (F .. F + 3) := "<N/>";
                  Result_Last := F + 3;
               when QUOTED_NULL_STRING  =>
                  Result (F .. F + 3) := "<Q/>";
                  Result_Last := F + 3;
               when UNSPLITABLE         =>
                  Result (F .. F + 7) := "<U>" & Source.Str (Index) & "</U>";
                  Result_Last := F + 7;
               when FIELD_SEP           =>
                  Result (F .. F + 7) := "<F>" & Source.Str (Index) & "</F>";
                  Result_Last := F + 7;
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
      Object.Str := Null_Str'Access;
      Object.Notes := Null_Notes'Access;
      Object.Last := 0;
      Object.Has_Null_String := False;
      Object.Ref_Counter := null;
      Object.Modified := null;
      Object.I_Am_Modifier := False;
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

      S_Length : constant Natural := Source.Str'Length;

   begin
      --  Ada.Text_IO.Put_Line
      --   (Chunk_Size'Img & "," & S_Length'Img & "," & Source.last'Img);
      if Chunk_Size > S_Length - Source.Last
        or else (Source.Modified.all and then not Source.I_Am_Modifier)
        or else (Is_Slice and then Source.Ref_Counter.all > 1)
      then
         declare
            New_Size : constant Positive :=
                         S_Length + Chunk_Size + (S_Length / Growth_Factor);

            New_Rounded_Up_Size : constant Positive :=
                                    ((New_Size - 1) / Min_Mul_Alloc + 1) *
                                       Min_Mul_Alloc;

            Tmp_Str : constant String_Access :=
              new String (1 .. New_Rounded_Up_Size);
            Tmp_Notes : constant Annotations_Access :=
              new Annotations (1 .. New_Rounded_Up_Size);
         begin
            --  Create new string and notes
            Tmp_Str (1 .. Source.Last) := Source.Str (1 .. Source.Last);
            Tmp_Notes (1 .. Source.Last) := Source.Notes (1 .. Source.Last);

            --  If not Null and reference counter to 1 then free
            if Source.Str /= Null_Str'Access then
               if Source.Ref_Counter.all = 1 then
                  Free (Source.Str);
                  Free (Source.Notes);
                  Source.Modified.all := False;
                  Source.I_Am_Modifier := False;
               else
                  Source.Ref_Counter.all := Source.Ref_Counter.all - 1;
                  Source.Ref_Counter := new Natural'(1);
                  Source.Modified := new Boolean'(False);
                  Source.I_Am_Modifier := False;
               end if;
            else
               Source.Ref_Counter := new Natural'(1);
               Source.Modified := new Boolean'(False);
               Source.I_Am_Modifier := False;
            end if;

            Source.Modified.all := False;
            Source.Str := Tmp_Str;
            Source.Notes := Tmp_Notes;
         end;
      else
         Source.I_Am_Modifier := True;
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
      Result : Annotated_String := Null_Annotated_String;
      Result_Length : constant Natural := Last - First + 1;
   begin
      if Result_Length = 0 then
         return Result;
      else
         Realloc_For_Chunk (Result, Result_Length, True);
         Result.Str (1 .. Result_Length) := Source.Str (First .. Last);
         Result.Notes (1 .. Result_Length) := Source.Notes (First .. Last);
         Result.Last := Result_Length;

         --  If the source has a null string we must scan it to see if the
         --  null_string occurs in the slice.
         if Source.Has_Null_String then
            for Index in First .. Last loop
               if Source.Notes (Index) = NULL_STRING then
                  Result.Has_Null_String := True;
                  exit;
               end if;
            end loop;
         end if;

         return Result;
      end if;
   end Slice;

   ---------
   -- Str --
   ---------

   function Str (Source : Annotated_String) return String is
   begin
      return Source.Str (1 .. Source.Last);
   end Str;

   -------------------------
   -- To_Annotated_String --
   -------------------------

   function To_Annotated_String
     (Source_Str : String; Source_Notes : Annotations) return Annotated_String
   is
      Result : Annotated_String;
   begin
      Result.Last := Source_Str'Length;
      Result.Str  := new String (1 .. Source_Str'Length);
      Result.Str.all := Source_Str;
      Result.Notes := new Annotations (1 .. Source_Str'Length);
      Result.Notes.all := Source_Notes;
      Result.Has_Null_String := False;
      Result.Ref_Counter := new Natural'(1);
      Result.Modified := new Boolean'(False);
      Result.I_Am_Modifier := False;
      return Result;
   end To_Annotated_String;

   -------------------------
   -- To_Annotated_String --
   -------------------------

   function To_Annotated_String
     (Source_Str : String; Note : Annotation) return Annotated_String
   is
      Result : Annotated_String;
   begin
      Result.Last := Source_Str'Length;
      Result.Str  := new String (1 .. Source_Str'Length);
      Result.Str.all := Source_Str;
      Result.Notes := new Annotations (1 .. Source_Str'Length);
      Result.Notes.all := (others => Note);
      Result.Has_Null_String := False;
      Result.Ref_Counter := new Natural'(1);
      Result.Modified := new Boolean'(False);
      Result.I_Am_Modifier := False;
      return Result;
   end To_Annotated_String;

end Posix_Shell.Annotated_Strings;
