with GNAT.OS_Lib;

package body Posix_Shell.Rm is

   -----------------
   -- Delete_File --
   -----------------

   function Delete_File (Filename : String) return long is

      Success : Boolean;
      Status : long := 0;
      pragma Unreferenced (Status);

   begin
      GNAT.OS_Lib.Delete_File (Filename, Success);
      if not Success then
         return 1;
      else
         return 0;
      end if;

   end Delete_File;

end Posix_Shell.Rm;
