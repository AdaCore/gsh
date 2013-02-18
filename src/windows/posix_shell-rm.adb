with System;
with Ada.Characters.Conversions;
with GNAT.OS_Lib;

package body Posix_Shell.Rm is

   -----------------
   -- Delete_File --
   -----------------

   function Delete_File (Filename : String) return long is

      function Delete_File_Internal
        (Filename : System.Address; Length : Integer)
         return long;
      pragma Import (C, Delete_File_Internal, "safe_unlink_interface");

      Norm_Path : constant String := GNAT.OS_Lib.Normalize_Pathname
        (Filename, Resolve_Links => False);
   begin
      return Delete_File_Internal
        (Ada.Characters.Conversions.To_Wide_String
           ("\??\" & Norm_Path)'Address, Norm_Path'Length + 4);
   end Delete_File;

end Posix_Shell.Rm;
