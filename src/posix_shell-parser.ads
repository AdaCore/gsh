with Posix_Shell.Tree; use Posix_Shell.Tree;

package Posix_Shell.Parser is

   function Parse_String (S : String) return Shell_Tree_Access;
   --  Same as above except that source is a string.

   function Parse_File (Filename : String) return Shell_Tree_Access;
   --  Same as above except that source is the content of file Filename.

end Posix_Shell.Parser;
