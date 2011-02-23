with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Annotated_String_Lists; use Annotated_String_Lists;
with Posix_Shell.Variables;  use Posix_Shell.Variables;

package Posix_Shell.Subst is

   function Eval_String
     (SS : Shell_State_Access;
      S : Annotated_String;
      Max_Split : Integer := -1)
      return String_List;

   function Eval_String_Unsplit
     (SS : Shell_State_Access;
      S : Annotated_String;
      Case_Pattern : Boolean := False;
      Quote_Removal_Only : Boolean := False)
      return String;

   function Eval_String_List
     (SS : Shell_State_Access; S : Annotated_String_List)
      return String_List;

end Posix_Shell.Subst;
