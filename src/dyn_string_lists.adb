
package body Dyn_String_Lists is

   -------
   -- & --
   -------

   function "&" (Left : String_Access;
                 Right : Dyn_String_List)
                 return Dyn_String_List
   is
      Result : Dyn_String_List := Null_String_List;
   begin
      Append (Result, Left);
      Append (Result, Right);
      return Result;
   end "&";

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : String_Access)
   is
      N : constant Node_Access := new Node'(Item, null);
   begin
      Source.Length := Source.Length + 1;
      if Source.First /= null then
         Source.Last.Next := N;
         Source.Last := N;
      else
         Source.First := N;
         Source.Last := N;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : String_List)
   is
   begin
      for J in Item'Range loop
         Append (Source, Item (J));
      end loop;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Dyn_String_List; Item : Dyn_String_List)
   is
   begin
      if Item.Length > 0 then
         Source.Length := Source.Length + Item.Length;
         if Source.First /= null then
            Source.Last.Next := Item.First;
            Source.Last := Item.Last;
         else
            Source.First := Item.First;
            Source.Last := Item.Last;
         end if;
      end if;
   end Append;

   -------------
   -- Content --
   -------------

   function Content
     (Source : Dyn_String_List) return String_List
   is
      Result : String_List (1 .. Length (Source));
      N      : Node_Access := Source.First;
   begin
      for J in 1 .. Length (Source) loop
         Result (J) := N.all.T;
         N := N.all.Next;
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
