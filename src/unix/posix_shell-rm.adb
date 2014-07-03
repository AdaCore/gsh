with GNAT.OS_Lib;
with GNAT.Directory_Operations;

package body Posix_Shell.Rm is

   -----------------
   -- Delete_File --
   -----------------

   function Delete_File (Filename : String) return long is

      Success : Boolean;
      Status : long := 0;
      pragma Unreferenced (Status);

   begin
      if GNAT.OS_Lib.Is_Directory (Filename) then
         begin
            GNAT.Directory_Operations.Remove_Dir (Filename);
            Success := True;
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Success := False;
         end;
      else
         GNAT.OS_Lib.Delete_File (Filename, Success);
      end if;
      if not Success then
         return 1;
      else
         return 0;
      end if;

   end Delete_File;

end Posix_Shell.Rm;
