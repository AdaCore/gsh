with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.Parser is

   function Parse_String (S : String) return Shell_Tree_Access;
   --  Same as above except that source is a string.

   function Parse_File (Filename : String) return Shell_Tree_Access;
   --  Same as above except that source is the content of file Filename.

   function Parse_Buffer (B : Buffer_Access) return Shell_Tree_Access;
   --  Same as above except that the source is directly a buffer.

end Posix_Shell.Parser;
