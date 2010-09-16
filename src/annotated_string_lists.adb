with Ada.Unchecked_Deallocation;

package body Annotated_String_Lists is

   procedure Free (X : in out Annotated_String_Array_Access);
   procedure Realloc_For_Chunk
     (Source     : in out Annotated_String_List;
      Chunk_Size : Natural);

   -------
   -- & --
   -------

   function "&" (Left : Annotated_String;
                 Right : Annotated_String_List)
                 return Annotated_String_List
   is
      Result : Annotated_String_List := Null_Annotated_String_List;
   begin
      Append (Result, Left);
      Append (Result, Right);
      return Result;
   end "&";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Annotated_String_List) is
      Old : constant Annotated_String_Array_Access := Object.Reference;
   begin
      if Old /= Null_Annotated_String_List.Reference then
         Object.Reference := new Annotated_String_Array'
           (Old (1 .. Object.Last));
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String_List; Item : Annotated_String)
   is
   begin
      Realloc_For_Chunk (Source, 1);
      Source.Reference (Source.Last + 1) := Item;
      Source.Last := Source.Last + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Annotated_String_List; Item : Annotated_String_List)
   is
   begin
      if Item.Last /= 0 then
         Realloc_For_Chunk (Source, Item.Last);
         Source.Reference (Source.Last + 1 .. Source.Last + Item.Last)
           := Item.Reference (1 .. Item.Last);
         Source.Last := Source.Last + Item.Last;
      end if;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Annotated_String_List;
      Index : Natural)
      return Annotated_String
   is
   begin
      return Source.Reference (Index);
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Annotated_String_List) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Annotated_String_Array,
           Annotated_String_Array_Access);

   begin
      if Object.Reference /= Null_Annotated_String_List.Reference then
         Deallocate (Object.Reference);
         Object.Reference := Null_Annotated_String_List.Reference;
         Object.Last := 0;
      end if;

   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Annotated_String_Array_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Annotated_String_Array,
           Annotated_String_Array_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Annotated_String_List.Reference then
         Deallocate (X);
      end if;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Annotated_String_List) is
   begin
      Object.Reference := Null_Annotated_String_List.Reference;
      Object.Last := 0;
   end Initialize;

   ------------
   -- Length --
   ------------

   function Length (Source : Annotated_String_List) return Integer
   is
   begin
      return Source.Last;
   end Length;

   -----------------------
   -- Realloc_For_Chunk --
   -----------------------

   procedure Realloc_For_Chunk
     (Source     : in out Annotated_String_List;
      Chunk_Size : Natural)
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

      S_Length : Natural;

   begin
      if Source.Reference /= Null_Annotated_String_List.Reference then
         S_Length := Source.Reference'Length;
      else
         S_Length := 0;
      end if;

      if Chunk_Size > S_Length - Source.Last then
         declare
            New_Size : constant Positive :=
                         S_Length + Chunk_Size + (S_Length / Growth_Factor);

            New_Rounded_Up_Size : constant Positive :=
                                    ((New_Size - 1) / Min_Mul_Alloc + 1) *
                                       Min_Mul_Alloc;

            Tmp : Annotated_String_Array_Access := null;

         begin
            Tmp := new Annotated_String_Array (1 .. New_Rounded_Up_Size);
            if Source.Reference /= Null_Annotated_String_List.Reference then
               Tmp (1 .. Source.Last) := Source.Reference (1 .. Source.Last);
               Free (Source.Reference);
            end if;
            Source.Reference := Tmp;
         end;
      end if;
   end Realloc_For_Chunk;

end Annotated_String_Lists;
