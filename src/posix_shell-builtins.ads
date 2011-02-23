with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Builtins is

   function Is_Builtin (Cmd : String) return Boolean;
   --  Return True if Cmd is a known builtin.

   function Execute_Builtin
     (S : Shell_State_Access; Cmd : String; Args : String_List) return Integer;
   --  Execute the given builtin.

end Posix_Shell.Builtins;
