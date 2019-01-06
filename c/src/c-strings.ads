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

with GNAT.Strings;
with System;

package C.Strings is

   package GS renames GNAT.Strings;

   type CList is limited private;
   --  List of strings. The structure is efficient dynamic allocation
   --  and allow export to various kind of C structures without need for
   --  reallocation.

   procedure Append (Self : in out CList; Item : String);
   --  Append a new element to the list
   --
   --  @param Self a string list buffer
   --  @param Item a new element

   procedure Append (Self : in out CList; Item : GS.String_List);
   --  Likewise with String_List

   procedure Append (Self : in out CList; Item : CList);
   --  Likewise with CList

   function Length (Self : CList) return Natural;
   pragma Inline (Length);
   --  Return number of elements stored
   --
   --  @param a String_List
   --  @return the number of elements

   function Element (Self : CList; Index : Positive) return String;
   pragma Inline (Element);
   --  Return Index(th) element of the list as a string

   procedure Deallocate (Self : in out CList);
   --  Free memory used by Self (reseting Self to the empty list).

   function As_List (Self : CList) return GS.String_List;
   --  Return a String_List representation

   function As_C_String_Array (Self : CList) return System.Address;
   --  Return the address to the "char**" representation of the
   --  structure.

   function As_C_String_Block (Self : CList) return System.Address;
   --  Return the address to a block of C strings. A block consists of
   --  a null-terminated block of null-terminated strings

   function Join
     (Self      : CList;
      Separator : String;
      Quote     : String)
      return String;

   Empty_CList : constant CList;
private

   type Indices is array (Natural range <>) of System.Address;
   type Indices_Access is access Indices;

   type CList is limited record
      Idx      : Indices_Access   := null;
      Str      : GS.String_Access := null;
      Idx_Last : Natural          := 0;
      Str_Last : Natural          := 0;
   end record;

   Empty_CList : constant CList := (null, null, 0, 0);

end C.Strings;
