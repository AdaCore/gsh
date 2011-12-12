with GNAT.Strings; use GNAT.Strings;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Utils is

   function To_String (I : Integer) return String;
   --  Convert an Integer to a string without leading spaces

   procedure To_Integer (S : String; V : out Integer; Valid : out Boolean);
   --  If the string "S" is a valid decimal representation of a number,
   --  return its value in "V", and set Valid to True. Otherwise, set
   --  "Valid" to False (in which case, the value of V is undefined).

   function Is_Natural (S : String) return Boolean;

   function To_Integer (B : Boolean) return Integer;
   --  Return the shell-version of True and False: If B is True, then
   --  return zero, else return 1.

   function Integer_Not (Value : Integer) return Integer;
   --  Performs the shell equivalent of a not: If Value is zero, then
   --  return 1. Else, return 0.

   function Current_Working_Directory (S : Shell_State) return String;
   --  Similar to GNAT.Directory_Operations.Get_Current_Dir except that
   --  the path returned always follows the UNIX convention of using
   --  forward slashes. Also, the directory name does not have a directory
   --  separator at the end of the string.
   --
   --  The reason for always following the UNIX convention is to facilitate
   --  the cygwin/mingwin interaction. We also remove the ending directory
   --  separator because this is what shells will typically do.

   function Locate_Exec
     (S : Shell_State; Exec_Name : String)
      return String_Access;
   --  This function correspond to the one implemented in GNAT.OS_Lib with the
   --  difference that it does not replace path components which are links

   function Strip_CR (Str : String) return String;
   --  This function removes all CR from a given string STR

   --  package Dynamic_String_Lists is new GNAT.Dynamic_Tables
   --  (String_Access,
   --   Natural,
   --   1,
   --   8,
   --   50);

   --  procedure Append
   --  (L : in out Dynamic_String_Lists.Instance;
   --   E : String_List);
   --  Append a list of string to a dynamic_string_list
end Posix_Shell.Utils;
