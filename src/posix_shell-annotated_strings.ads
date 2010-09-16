--  This package provides an interface to strings structure with embeded
--  annotations called annotated_string. Basically to each character of the
--  string there is a corresponding annotation. Note that Annotated_String is
--  a controlled record and its size is not static.
--  Annotated_String indexes are always starting at index 1

with Ada.Finalization;

package Posix_Shell.Annotated_Strings is

   type Annotated_String is private;

   Null_Annotated_String : constant Annotated_String;

   function To_Annotated_String
     (Source_Str : String; Source_Notes : Annotations) return Annotated_String;
   --  Returned the associated annotated string whose value is Source_Str and
   --  annotations are Source_Notes. Source_Notes and Source_Str should have
   --  the same size. The Nth annotation of Source_Notes will be the
   --  annotation of the Nth character in Source_Str

   function To_Annotated_String
     (Source_Str : String; Note : Annotation) return Annotated_String;
   --  Returned an annotated string whose value is Source_Str. All characters
   --  will have annotation Note.

   function Str (Source : Annotated_String) return String;
   --  Return the Annotated_String string value

   procedure Append
     (Source : in out Annotated_String; C : Character; A : Annotation);
   --  Append to Source a character C with A as annotation

   procedure Append (Source : in out Annotated_String;
                     S : String;
                     A : Annotation);
   --  Append to Source a string S for which all characters will have A as
   --  annotation

   procedure Append
     (Source : in out Annotated_String; New_Item : Annotated_String);
   --  Append New_Item to Source

   function Get_Annotation
     (Source : Annotated_String; Index : Positive)
      return Annotation;
   --  Get annotation of the Nth character.

   function Get_Character
     (Source : Annotated_String; Index : Positive)
      return Character;
   --  Get Nth character.

   function Slice
     (Source : Annotated_String; First, Last : Natural)
     return Annotated_String;
   --  Return a slice of Source.

   function Length (Source : Annotated_String) return Natural;
   --  Return length of Source.

   function Has_Null_String (Source : Annotated_String) return Boolean;
   --  Return true if Source contains a NULL_STRING annotation

   function Image (Source : Annotated_String) return String;
   --  Return the string image of an annotated string

private
   use Ada.Finalization;

   type Boolean_Access is access Boolean;
   type Natural_Access is access Natural;

   type Annotations_Access is access all Annotations;
   type Annotation_Records_Access is access all Annotation_Records;

   Null_Str    : aliased String := "";
   Null_Notes  : aliased Annotations := (1 .. 0 => NO_ANNOTATION);
   Null_Note_Records : aliased Annotation_Records :=
     (1 .. 0 => (NO_ANNOTATION, 0));
   type String_Access is access all String;

   type Annotated_String is new Controlled with record
      Str             : String_Access := Null_Str'Access;
      Notes           : Annotations_Access := Null_Notes'Access;
      Last            : Natural := 0;
      Has_Null_String : Boolean := False;
      Ref_Counter     : Natural_Access := null;
      Modified        : Boolean_Access := null;
      I_Am_Modifier   : Boolean := False;
   end record;

   pragma Finalize_Storage_Only (Annotated_String);

   procedure Initialize (Object : in out Annotated_String);
   procedure Adjust     (Object : in out Annotated_String);
   procedure Finalize   (Object : in out Annotated_String);

   Null_Annotated_String : constant Annotated_String :=
     (Controlled with
      Str => Null_Str'Access,
      Notes => Null_Notes'Access,
      Last => 0,
      Has_Null_String => False,
      Ref_Counter => null,
      Modified => null,
      I_Am_Modifier => False);

end Posix_Shell.Annotated_Strings;
