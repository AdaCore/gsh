------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

--  This package provides an interface to strings structure with embeded
--  annotations called annotated_string. Basically it behaves like a string
--  except that sometimes we have some annotations insted of characters.
--  Note that Annotated_String is a controlled record and its size is not
--  static. Annotated_String indexes are always starting at index 1

package Sh.Annotated_Strings is

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

   function Length (Source : Annotated_String) return Natural;
   --  Return length of Source.

   function Image (Source : Annotated_String) return String;
   --  Return the string image of an annotated string

   procedure Deallocate (Object : in out Annotated_String);

private

   type Boolean_Access is access Boolean;
   type Natural_Access is access Natural;

   type Annotations_Access is access all Annotations;
   type Annotation_Records_Access is access all Annotation_Records;

   type Str_Elements is array (Natural range <>) of Str_Element;
   type Str_Elements_Access is access all Str_Elements;

   type Annotated_String is record
      Buffer      : Str_Elements_Access := null;
      Last        : Natural := 0;
   end record;

end Sh.Annotated_Strings;
