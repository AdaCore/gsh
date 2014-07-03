with GNAT.Strings; use GNAT.Strings;

package Dyn_String_Lists is

   type Dyn_String_List is private;

   procedure Append
     (Source : in out Dyn_String_List; Item : String_Access);

   procedure Append
     (Source : in out Dyn_String_List; Item : String_List);

   procedure Append
     (Source : in out Dyn_String_List; Item : Dyn_String_List);

   function "&"
     (Left  : String_Access;
      Right : Dyn_String_List)
      return Dyn_String_List;

   function Length (Source : Dyn_String_List) return Integer;

   function Content
     (Source : Dyn_String_List) return String_List;

private

   type Node;
   type Node_Access is access Node;

   type Node is record
      T      : String_Access;
      Next   : Node_Access := null;
   end record;

   type Dyn_String_List is record
      First  : Node_Access := null;
      Last   : Node_Access := null;
      Length : Natural := 0;
   end record;

   Null_String_List : constant Dyn_String_List := (null, null, 0);

end Dyn_String_Lists;
