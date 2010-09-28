
package Posix_Shell.Parser is

   function Parse_String (S : String) return Node_Id;
   --  Same as above except that source is a string.

   function Parse_File (Filename : String) return Node_Id;
   --  Same as above except that source is the content of file Filename.

end Posix_Shell.Parser;
