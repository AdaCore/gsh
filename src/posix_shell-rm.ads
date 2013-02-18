with Interfaces.C; use Interfaces.C;

package Posix_Shell.Rm is

   function Delete_File (Filename : String) return long;
   --  Delete file Filename and return 0 on success and a Windows error code
   --  otherwise

end Posix_Shell.Rm;
