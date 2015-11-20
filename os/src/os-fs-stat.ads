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

--  This unit provides various interface to the filesystem

package OS.FS.Stat is

   type File_Attributes is private;
   --  Record containing information about a file or directory on the
   --  filesystem

   function File_Information (Path : String) return File_Attributes;
   --  Retrieve file information about a file located at Path

   function Image (FI : File_Attributes) return String;
   --  String image of a File_Attributes structure

   function Exists (FI : File_Attributes) return Boolean;
   --  Return True if the file exist on the filesystem

   function Is_Regular_File (FI : File_Attributes) return Boolean;
   --  Return True if the file is a regular file

   function Is_Directory (FI : File_Attributes) return Boolean;
   --  Return True if the file is a directory

   function Is_Symbolic_Link (FI : File_Attributes) return Boolean;
   --  Return True if the file is a symbolic link

private

   type File_Attributes is record
      Error             : Integer;
      Exists            : Boolean;
      Writable          : Boolean;
      Readable          : Boolean;
      Executable        : Boolean;
      Symbolic_Link     : Boolean;
      Regular           : Boolean;
      Directory         : Boolean;
      Stamp             : Long_Long_Integer;
      Length            : Long_Long_Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, File_Attributes);

   pragma Inline (Exists);
   pragma Inline (Is_Regular_File);
   pragma Inline (Is_Directory);
   pragma Inline (Is_Symbolic_Link);

end OS.FS.Stat;
