------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2015, AdaCore                   --
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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body OS.FS.Stat is

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information (Path : String) return File_Attributes
   is
      function Internal (Path : chars_ptr) return File_Attributes;
      pragma Import (C, Internal, "__gsh_file_information");
      Name_Ptr : chars_ptr := New_String (Path);
      Result   : File_Attributes;
   begin
      Result := Internal (Name_Ptr);
      Free (Name_Ptr);
      return Result;
   end File_Information;

   -----------
   -- Image --
   -----------

   function Image (FI : File_Attributes) return String
   is
   begin
      return "exits: " & FI.Exists'Img &
        ", writable: " & FI.Writable'Img &
        ", readable: " & FI.Readable'Img &
        ", executable: " & FI.Executable'Img &
        ", regular: " & FI.Regular'Img &
        ", directory: " & FI.Directory'Img;
   end Image;

   ------------
   -- Exists --
   ------------

   function Exists (FI : File_Attributes) return Boolean
   is
   begin
      return FI.Exists;
   end Exists;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (FI : File_Attributes) return Boolean
   is
   begin
      return FI.Regular;
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (FI : File_Attributes) return Boolean
   is
   begin
      return FI.Directory;
   end Is_Directory;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (FI : File_Attributes) return Boolean
   is
   begin
      return FI.Symbolic_Link;
   end Is_Symbolic_Link;

end OS.FS.Stat;
