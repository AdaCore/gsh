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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;

package body Posix_Shell.Rm is

   -----------------
   -- Delete_File --
   -----------------

   function Delete_File (Filename : String) return long is

      function Delete_File_Internal
        (Path : chars_ptr)
         return long;
      pragma Import (C, Delete_File_Internal, "__gsh_unlink");

      Norm_Path : constant String := GNAT.OS_Lib.Normalize_Pathname
        (Filename, Resolve_Links => False);
      Name_Ptr  : chars_ptr := New_String (Norm_Path);
      Result : long;
   begin
      Result := Delete_File_Internal (Name_Ptr);
      Free (Name_Ptr);
      return Result;
   end Delete_File;

end Posix_Shell.Rm;
