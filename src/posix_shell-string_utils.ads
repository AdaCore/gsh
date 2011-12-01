package Posix_Shell.String_Utils is

   function Starts_With (S : String; Sub : String) return Boolean;
   --  return True if S starts with Sub, False otherwise.

   function Last_Lines
     (S : String; Size : Natural) return String;
   --  return the last Size lines of S

   function From_Line
     (S : String; LineNo : Natural) return String;
   --  return the substring starting at line LineNo ending at end of S

   function To_Line
     (S : String; LineNo : Natural) return String;
   --  return the substrubg till line LIneNo of S

end Posix_Shell.String_Utils;
