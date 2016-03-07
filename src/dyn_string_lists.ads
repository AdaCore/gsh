------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with GNAT.Strings; use GNAT.Strings;

package Dyn_String_Lists is

   type Dyn_String_List is private;

   procedure Append
     (Source : in out Dyn_String_List; Item : String);

   procedure Append
     (Source : in out Dyn_String_List; Item : String_List);

   procedure Append
     (Source : in out Dyn_String_List; Item : Dyn_String_List);

   procedure Prepend
     (Source : in out Dyn_String_List; Item : String);

   function Length (Source : Dyn_String_List) return Integer;

   function Content
     (Source : Dyn_String_List) return String_List;

private

   type Dyn_String_List is record
      Content       : String_Access := null;
      Length        : Natural := 0;
      Size          : Natural := 0;
      Max_Size      : Natural := 0;
   end record;

   Null_String_List : constant Dyn_String_List :=
     (null, 0, 0, 0);

end Dyn_String_Lists;
