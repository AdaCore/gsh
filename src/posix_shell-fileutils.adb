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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;

package body Posix_Shell.Fileutils is

   -----------
   -- Close --
   -----------

   procedure Close (D : Dir_Handle) is
      procedure Internal (D : Dir_Handle);
      pragma Import (C, Internal, "__gsh_close_directory");
   begin
      Internal (D);
   end Close;

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

   ----------
   -- Open --
   ----------

   function Open (Path : String) return Dir_Handle
   is
      function Internal (Path : chars_ptr)
                         return System.Address;
      pragma Import (C, Internal, "__gsh_open_directory");
      Name_Ptr : chars_ptr := New_String (Path);
      Handle : System.Address;
   begin
      Handle := Internal (Name_Ptr);
      Free (Name_Ptr);
      return Dir_Handle (Handle);
   end Open;

   ----------
   -- Read --
   ----------

   function Read (D : Dir_Handle) return Dir_Entry
   is
      function Internal (Handle : System.Address) return Dir_Entry;
      pragma Import (C, Internal, "__gsh_next_entry");
      Result : Dir_Entry;
   begin
      Result := Internal (System.Address (D));
      while Name (Result) = ".." or else Name (Result) = "." loop
         Result := Internal (System.Address (D));
      end loop;
      return Result;
   end Read;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information (DE : Dir_Entry) return File_Attributes
   is
   begin
      return DE.Info;
   end File_Information;

   ----------
   -- Name --
   ----------

   function Name (DE : Dir_Entry) return String
   is
      Result : constant String := To_Ada (DE.Name);
   begin
      return Result;
   end Name;

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

   -------------
   -- Is_Null --
   -------------

   function Is_Null (DE : Dir_Entry) return Boolean
   is
   begin
      return DE.Name (0) = Interfaces.C.char (ASCII.NUL);
   end Is_Null;

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

end Posix_Shell.Fileutils;
