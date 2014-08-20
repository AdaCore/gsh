------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

--  This package provides an interface to strings structure with embeded
--  annotations called annotated_string. Basically it behaves like a string
--  except that sometimes we have some annotations insted of characters.
--  Note that Annotated_String is a controlled record and its size is not
--  static. Annotated_String indexes are always starting at index 1

with Ada.Finalization;

package Posix_Shell.Annotated_Strings is

   type Str_Element_Kind is
     (E_CHAR,
      E_CTRL,
      E_NULL);
   --  Element in an annotated string can be either a regular character, an
   --  annotation or null (??? should we remove null)

   type Str_Element (Kind : Str_Element_Kind := E_NULL) is record
      case Kind is
         when E_CHAR =>
            Char : Character;
         when E_CTRL =>
            Ctrl : Annotation;
         when E_NULL =>
            null;
      end case;
   end record;

   type Annotated_String is private;

   Null_Annotated_String : constant Annotated_String;

   function Str (Source : Annotated_String) return String;
   --  Return the Annotated_String string value

   procedure Append
     (Source : in out Annotated_String; C : Character);
   --  Append to Source a character C

   procedure Append
     (Source : in out Annotated_String; A : Annotation);
   --  Append to Source an annotation

   procedure Append
     (Source : in out Annotated_String; S : String);
   --  Append to Source an annotation

   procedure Append
     (Source : in out Annotated_String; New_Item : Annotated_String);
   --  Append New_Item to Source

   function Get_Element
     (Source : Annotated_String; Index : Positive)
      return Str_Element;
   --  Get Nth element.

   function Slice
     (Source : Annotated_String; First, Last : Natural)
     return Annotated_String;
   --  Return a slice of Source.

   function Length (Source : Annotated_String) return Natural;
   --  Return length of Source.

   function Image (Source : Annotated_String) return String;
   --  Return the string image of an annotated string

private
   use Ada.Finalization;

   type Boolean_Access is access Boolean;
   type Natural_Access is access Natural;

   type Annotations_Access is access all Annotations;
   type Annotation_Records_Access is access all Annotation_Records;

   type Str_Elements is array (Natural range <>) of Str_Element;
   type Str_Elements_Access is access all Str_Elements;

   Null_Element : aliased Str_Element := (Kind => E_NULL);
   Null_Elements : aliased Str_Elements :=
     (1 .. 0 => Null_Element);

   type Annotated_String is new Controlled with record
      Buffer      : Str_Elements_Access := Null_Elements'Access;
      Last        : Natural := 0;
      Ref_Counter : Natural_Access := null;
      Modified    : Boolean_Access := null;
      Is_Modifier : Boolean := False;
   end record;

   pragma Finalize_Storage_Only (Annotated_String);

   procedure Initialize (Object : in out Annotated_String);
   procedure Adjust     (Object : in out Annotated_String);
   procedure Finalize   (Object : in out Annotated_String);

   Null_Annotated_String : constant Annotated_String :=
     (Controlled with
      Buffer      => Null_Elements'Access,
      Last        => 0,
      Ref_Counter => null,
      Modified    => null,
      Is_Modifier => False);

end Posix_Shell.Annotated_Strings;