with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Commands_Preprocessor is

   function Run
     (S : Shell_State_Access;
      Cmd : String; Args : String_List; Env : String_List)
      return Integer;
   --  Run the given command with the arguments provided, and
   --  return its exit status.

end Posix_Shell.Commands_Preprocessor;
