with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Tree; use Posix_Shell.Tree;

package Posix_Shell.Parser is

   Null_String_List : String_List (1 .. 0);

   Null_String_List_Access : String_List_Access :=
     new String_List'(Null_String_List);

   function Parse_String (S : String) return Node_Id;
   --  Same as above except that source is a string.

   function Parse_File (Filename : String) return Node_Id;
   --  Same as above except that source is the content of file Filename.

private
   Next_Node : Node_Id := 1;
end Posix_Shell.Parser;
