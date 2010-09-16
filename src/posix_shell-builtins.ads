with GNAT.OS_Lib; use GNAT.OS_Lib;

package Posix_Shell.Builtins is

   function Is_Builtin (Cmd : String) return Boolean;
   --  Return True if Cmd is a known builtin.

   function Execute_Builtin (Cmd : String; Args : String_List) return Integer;
   --  Execute the given builtin.

end Posix_Shell.Builtins;
