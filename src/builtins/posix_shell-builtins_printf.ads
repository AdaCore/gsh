with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Variables; use Posix_Shell.Variables;
package Posix_Shell.Builtins_Printf is

   function Builtin_Printf
     (S : Shell_State_Access; Args : String_List) return Integer;
   --  Implement the "printf" builtin.

end Posix_Shell.Builtins_Printf;
