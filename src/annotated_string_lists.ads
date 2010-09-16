with Ada.Finalization;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;

package Annotated_String_Lists is

   type Annotated_String_List is private;
   Null_Annotated_String_List : constant Annotated_String_List;

   procedure Append
     (Source : in out Annotated_String_List; Item : Annotated_String);

   procedure Append
     (Source : in out Annotated_String_List; Item : Annotated_String_List);

   function "&" (Left : Annotated_String;
                 Right : Annotated_String_List)
                 return Annotated_String_List;

   function Length (Source : Annotated_String_List) return Integer;

   function Element
     (Source : Annotated_String_List;
      Index : Natural)
      return Annotated_String;

private
   use Ada.Finalization;

   type Annotated_String_Array is array (Natural range <>) of Annotated_String;
   type Annotated_String_Array_Access is access all Annotated_String_Array;

   type Annotated_String_List is new Controlled with record
      Reference   : Annotated_String_Array_Access := null;
      Last  : Natural := 0;
   end record;

   pragma Finalize_Storage_Only (Annotated_String_List);

   procedure Initialize (Object : in out Annotated_String_List);
   procedure Adjust     (Object : in out Annotated_String_List);
   procedure Finalize   (Object : in out Annotated_String_List);

   Null_Annotated_String_List : constant Annotated_String_List :=
     (Controlled with
      Reference => null,
      Last => 0);
end Annotated_String_Lists;
