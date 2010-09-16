with Ada.Finalization;
with GNAT.Strings; use GNAT.Strings;

package Dyn_String_Lists is

   type Dyn_String_List is private;

   procedure Append
     (Source : in out Dyn_String_List; Item : String_Access);

   procedure Append
     (Source : in out Dyn_String_List; Item : String_List);

   procedure Append
     (Source : in out Dyn_String_List; Item : Dyn_String_List);

   function "&" (Left  : String_Access;
                 Right : Dyn_String_List)
                 return Dyn_String_List;

   function Length (Source : Dyn_String_List) return Integer;

   function Element
     (Source : Dyn_String_List;
      Index : Natural)
      return String_Access;

   function Content
     (Source : Dyn_String_List) return String_List;

private
   use Ada.Finalization;

   type Dyn_String_Array_Access is access all String_List;

   type Dyn_String_List is new Controlled with record
      Reference   : Dyn_String_Array_Access := null;
      Last        : Natural := 0;
   end record;

   pragma Finalize_Storage_Only (Dyn_String_List);

   procedure Initialize (Object : in out Dyn_String_List);
   procedure Adjust     (Object : in out Dyn_String_List);
   procedure Finalize   (Object : in out Dyn_String_List);

end Dyn_String_Lists;
