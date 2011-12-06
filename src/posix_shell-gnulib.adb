with System;

package body Posix_Shell.GNULib is

   function C_Fnmatch
     (Pattern : System.Address; Str : System.Address)
      return Integer;
   pragma Import (C, C_Fnmatch, "gsh_fnmatch");

   -------------
   -- Fnmatch --
   -------------

   function Fnmatch (Pattern : String; Str : String) return Boolean is
      C_Pattern : aliased String (1 .. Pattern'Length + 1);
      C_Str : aliased String (1 .. Str'Length + 1);
      Result : Integer;
   begin
      C_Pattern (1 .. Pattern'Length) := Pattern;
      C_Pattern (C_Pattern'Last) := ASCII.NUL;
      C_Str (1 .. Str'Length) := Str;
      C_Str (C_Str'Last) := ASCII.NUL;

      Result := C_Fnmatch (C_Pattern'Address, C_Str'Address);
      if Result = 0 then
         return True;
      else
         return False;
      end if;

   end Fnmatch;

end Posix_Shell.GNULib;
