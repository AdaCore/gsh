with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Annotated_String_Lists; use Annotated_String_Lists;
with Posix_Shell.Buffers; use Posix_Shell.Buffers;

with GNAT.Dynamic_Tables;

package Posix_Shell.Tree is

   type Shell_Tree is private;
   type Shell_Tree_Access is access Shell_Tree;

   type List_Kind is (OR_LIST, AND_LIST);

   type And_Or_Node_Id is record
      N    : Node_Id;
      Kind : List_Kind;
   end record;

   Null_And_Or_Node : constant And_Or_Node_Id := (0, OR_LIST);

   package Node_Id_Tables is new GNAT.Dynamic_Tables
     (Node_Id,
      Positive,
      1,
      128,
      100);

   package And_Or_Node_Id_Tables is new GNAT.Dynamic_Tables
     (And_Or_Node_Id,
      Positive,
      1,
      16,
      100);

   type Node_Id_Array is array (Positive range <>) of Node_Id;
   type And_Or_Node_Id_Array is array (Positive range <>) of And_Or_Node_Id;

   --------------------
   -- Tree modifiers --
   --------------------

   --  The following functions are dealing with the addition or modification of
   --  nodes in a tree.

   function Add_List_Node
     (Tree   : Shell_Tree_Access;
      Childs : Node_Id_Array)
      return Node_Id;
   --  Create a node that represent a list of ; or newline separated statements
   --  Childs is the list of nodes that goes into that sequence.

   function Add_And_Or_List_Node
     (Tree   : Shell_Tree_Access;
      Childs : And_Or_Node_Id_Array)
      return Node_Id;
   --  Create a node that represent a list of statements separated by || or &&

   function Add_Pipe_Node
     (Tree          : Shell_Tree_Access;
      Childs : Node_Id_Array;
      Pipe_Negation : Boolean)
      return Node_Id;

   function Add_Until_Node
     (Tree            : Shell_Tree_Access;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_While_Node
     (Tree            : Shell_Tree_Access;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_If_Node
     (Tree                        : Shell_Tree_Access;
      Cond, True_Code, False_Code : Node_Id)
      return Node_Id;

   function Add_Case_Node
     (Tree   : Shell_Tree_Access;
      Case_Value     : Annotated_String;
      Case_List_Code : Node_Id)
      return Node_Id;

   function Add_Case_List_Node
     (Tree   : Shell_Tree_Access;
      Pattern        : Annotated_String_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id)
      return Node_Id;

   function Add_For_Node
     (Tree   : Shell_Tree_Access;
      Variable_Name : Annotated_String;
      Value_List    : Annotated_String_List;
      Loop_Code     : Node_Id)
      return Node_Id;

   function Add_Brace_Node
     (Tree   : Shell_Tree_Access;
      Brace_Code : Node_Id) return Node_Id;

   function Add_Subshell_Node
     (Tree   : Shell_Tree_Access;
      Subshell_Code : Node_Id) return Node_Id;

   function Add_Null_Node (Tree : Shell_Tree_Access) return Node_Id;

   procedure Set_Cmd_Node
     (Tree   : Shell_Tree_Access;
      N      : Node_Id;
      Cmd    : Annotated_String);

   procedure Set_Function_Node
     (Tree          : Shell_Tree_Access;
      N             : Node_Id;
      Name          : Annotated_String;
      Function_Code : Shell_Tree_Access);

   procedure Set_Node_Redirection
     (Tree      : Shell_Tree;
      N         : Node_Id;
      Target_FD : Natural;
      Filename  : Annotated_String;
      Source_FD : Natural;
      Cmd       : Redir_Cmd);

   procedure Append_Arg
     (Tree   : Shell_Tree;
      N      : Node_Id;
      S      : Annotated_String);

   procedure Append_Assignement
     (Tree   : Shell_Tree_Access;
      N      : Node_Id;
      S      : Annotated_String);

   procedure Set_Node_Pos
     (Tree   : Shell_Tree;
      N : Node_Id;
      Pos : Text_Position);

   procedure Free_Node
     (Tree   : Shell_Tree_Access; N : Node_Id);

   procedure Free_Node
     (Tree   : Shell_Tree_Access);

   function New_Tree return Shell_Tree_Access;

   procedure Set_Tree_Toplevel (Tree : Shell_Tree_Access; N : Node_Id);
   type Node is private;
   type Node_Access is access Node;

private

   type Node_Kind is
     (IF_NODE,
      LIST_NODE,
      AND_OR_LIST_NODE,
      PIPE_NODE,
      FOR_NODE,
      CASE_NODE,
      CASE_LIST_NODE,
      UNTIL_NODE,
      WHILE_NODE,
      BRACE_NODE,
      SUBSHELL_NODE,
      FUNCTION_NODE,
      ASSIGN_NODE,
      CMD_NODE,
      NULL_CMD_NODE);

   type And_Or_Record is record
      N    : Node_Access;
      Kind : List_Kind;
   end record;

   type Node_Triplet is array (1 .. 3) of Node_Access;
   type Node_Id_Array_Access is access Node_Id_Array;
   type And_Or_Node_Id_Array_Access is access And_Or_Node_Id_Array;

   package Node_Tables is new GNAT.Dynamic_Tables
     (Node_Access,
      Positive,
      1,
      128,
      100);

   use Node_Tables;

   type Shell_Tree is record
      Node_Table    : Instance;
      Next_Node     : Node_Id := 1;
      Toplevel_Node : Node_Id := 0;
   end record;

   function Get_Node (Tree : Shell_Tree; N : Node_Id) return Node_Access;

   type Node (Kind : Node_Kind := IF_NODE) is record
      Redirections      : Redirection_Op_Stack := Empty_Redirection_Op_Stack;
      Pos               : Text_Position := Null_Text_Position;
      case Kind is
         when IF_NODE =>
            Cond       : Node_Id;
            True_Code  : Node_Id;
            False_Code : Node_Id;
         when LIST_NODE =>
            List_Childs : Node_Id_Array_Access;
         when AND_OR_LIST_NODE =>
            And_Or_List_Childs : And_Or_Node_Id_Array_Access;
         when BRACE_NODE =>
            Brace_Code : Node_Id;
         when CASE_LIST_NODE =>
            Pattern_List  : Annotated_String_List;
            Match_Code    : Node_Id;
            Next_Patterns : Node_Id;
         when CASE_NODE =>
            Case_Word     : Annotated_String;
            First_Case    : Node_Id;
         when FOR_NODE =>
            Loop_Var        : Annotated_String;
            Loop_Var_Values : Annotated_String_List;
            Loop_Code       : Node_Id;
         when NULL_CMD_NODE =>
            null;
         when PIPE_NODE =>
            Pipe_Childs : Node_Id_Array_Access;
            Pipe_Negation : Boolean;
         when SUBSHELL_NODE =>
            Subshell_Code : Node_Id;
         when UNTIL_NODE =>
            Until_Cond : Node_Id;
            Until_Code : Node_Id;
         when WHILE_NODE =>
            While_Cond : Node_Id;
            While_Code : Node_Id;
         when FUNCTION_NODE =>
            Function_Name : Annotated_String;
            Function_Code : Shell_Tree_Access;
         when CMD_NODE =>
            Cmd : Annotated_String;
            Arguments : Annotated_String_List;
            Cmd_Assign_List : Annotated_String_List;
         when ASSIGN_NODE =>
            Assign_List : Annotated_String_List;
      end case;
   end record;

end Posix_Shell.Tree;
