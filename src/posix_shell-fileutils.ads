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

--  This unit provides various interface to the filesystem

with System;
with Interfaces.C;

package Posix_Shell.Fileutils is

   type File_Attributes is private;
   --  Record containing information about a file or directory on the
   --  filesystem

   type Dir_Entry is private;
   --  Record containing information about a directory entry

   type Dir_Handle is new System.Address;
   --  Handle on directory

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

   function Open (Path : String) return Dir_Handle;
   --  Open a directory in order to list its entries

   procedure Close (D : Dir_Handle);
   --  Close an handle to a directory

   function Read (D : Dir_Handle) return Dir_Entry;
   --  Read the next entry in a directory. A Null Dir_Entry will be returned if
   --  there is no more entry available.

   function Is_Null (DE : Dir_Entry) return Boolean;
   --  Return True if the entry is a null entry

   function File_Information (DE : Dir_Entry) return File_Attributes;
   --  Get File_Attributes of the directory entry DE

   function Name (DE : Dir_Entry) return String;
   --  Get the name of the directory entry DE

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

   type Dir_Entry is record
      Info : File_Attributes;
      Name : Interfaces.C.char_array (0 .. 511);
   end record;

   pragma Inline (Exists);
   pragma Inline (Is_Regular_File);
   pragma Inline (Is_Directory);
   pragma Inline (Is_Symbolic_Link);

end Posix_Shell.Fileutils;
