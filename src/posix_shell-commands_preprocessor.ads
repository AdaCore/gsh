with GNAT.OS_Lib; use GNAT.OS_Lib;

package Posix_Shell.Commands_Preprocessor is

   function Run
     (Cmd : String; Args : String_List; Env : String_List)
      return Integer;
   --  Run the given command with the arguments provided, and
   --  return its exit status.

end Posix_Shell.Commands_Preprocessor;
