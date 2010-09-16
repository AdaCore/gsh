with Ada.Unchecked_Deallocation;
--  with Ada.Text_IO;

package body Posix_Shell.Tree is

   function Add_Node (N : Node_Access) return Node_Id;

   procedure Free_Node (N : in out Node_Access);

   procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Node,
           Node_Access);

   -----------------------
   -- Add_And_Or_List_Node --
   -----------------------

   function Add_And_Or_List_Node
     (Childs : And_Or_Node_Id_Array)
      return Node_Id
   is
      Child_List : constant And_Or_Node_Id_Array_Access :=
        new And_Or_Node_Id_Array'(Childs);
   begin
      return Add_Node
        (new Node'(Kind => AND_OR_LIST_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   And_Or_List_Childs => Child_List));
   end Add_And_Or_List_Node;

   --------------------
   -- Add_Brace_Node --
   --------------------

   function Add_Brace_Node (Brace_Code : Node_Id) return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => BRACE_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Brace_Code => Brace_Code));
   end Add_Brace_Node;

   ------------------------
   -- Add_Case_List_Node --
   ------------------------

   function Add_Case_List_Node
     (Pattern        : Annotated_String_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => CASE_LIST_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Pattern_List => Pattern,
                   Match_Code => Case_Code,
                   Next_Patterns => Next_Case_Code));
   end Add_Case_List_Node;

   -------------------
   -- Add_Case_Node --
   -------------------

   function Add_Case_Node
     (Case_Value     : Annotated_String;
      Case_List_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => CASE_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Case_Word => Case_Value,
                   First_Case => Case_List_Code));
   end Add_Case_Node;

   ------------------
   -- Add_For_Node --
   ------------------

   function Add_For_Node
     (Variable_Name : Annotated_String;
      Value_List    : Annotated_String_List;
      Loop_Code     : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => FOR_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Loop_Var => Variable_Name,
                   Loop_Var_Values => Value_List,
                   Loop_Code => Loop_Code));
   end Add_For_Node;

   -----------------
   -- Add_If_Node --
   -----------------

   function Add_If_Node (Cond, True_Code, False_Code : Node_Id) return Node_Id
   is
   begin
      return Add_Node
        (new Node'
           (Kind => IF_NODE,
            Cond => Cond,
            True_Code => True_Code,
            False_Code => False_Code,
            Redirections => Empty_Redirection_Op_Stack,
            Pos => Null_Text_Position));

   end Add_If_Node;

   -------------------
   -- Add_List_Node --
   -------------------

   function Add_List_Node (Childs : Node_Id_Array) return Node_Id is
      Child_List : constant Node_Id_Array_Access := new Node_Id_Array'(Childs);
   begin
      return Add_Node
        (new Node'(Kind => LIST_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   List_Childs => Child_List));
   end Add_List_Node;

   --------------
   -- Add_Node --
   --------------

   function Add_Node (N : Node_Access) return Node_Id is
   begin
      Set_Item (Node_Table, Next_Node, N);
      Next_Node := Next_Node + 1;
      return Next_Node - 1;
   end Add_Node;

   -------------------
   -- Add_Null_Node --
   -------------------

   function Add_Null_Node return Node_Id is
   begin
      return Add_Node
        (new Node'(Kind => NULL_CMD_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position));
   end Add_Null_Node;

   -------------------
   -- Add_Pipe_Node --
   -------------------

   function Add_Pipe_Node (Left, Right : Node_Id; Pipe_Negation : Boolean)
                           return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => PIPE_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Pipe_Left => Left,
                   Pipe_Right => Right,
                   Pipe_Negation => Pipe_Negation));
   end Add_Pipe_Node;

   -----------------------
   -- Add_Subshell_Node --
   -----------------------

   function Add_Subshell_Node (Subshell_Code : Node_Id) return Node_Id
   is
   begin
      return Add_Node
        (new Node'(Kind => SUBSHELL_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Subshell_Code => Subshell_Code));
   end Add_Subshell_Node;

   --------------------
   -- Add_Until_Node --
   --------------------

   function Add_Until_Node (Cond, Loop_Code : Node_Id) return Node_Id is
   begin
      return Add_Node
        (new Node'(Kind => UNTIL_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   Until_Cond => Cond,
                   Until_Code => Loop_Code));
   end Add_Until_Node;

   --------------------
   -- Add_While_Node --
   --------------------

   function Add_While_Node (Cond, Loop_Code : Node_Id) return Node_Id is
   begin
      return Add_Node
        (new Node'(Kind => WHILE_NODE,
                   Redirections => Empty_Redirection_Op_Stack,
                   Pos => Null_Text_Position,
                   While_Cond => Cond,
                   While_Code => Loop_Code));
   end Add_While_Node;

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg
     (N    : Node_Id;
      S    : Annotated_String)
   is
      Current : constant Node_Access := Node_Table.Table (N);
   begin
      Append (Current.Arguments, S);
   end Append_Arg;

   ------------------------
   -- Append_Assignement --
   ------------------------

   procedure Append_Assignement
     (N    : Node_Id;
      S    : Annotated_String)
   is
      Current : Node_Access := Node_Table.Table (N);
      Assign_List : Annotated_String_List;
   begin
      if Current.Kind /= NULL_CMD_NODE then
         Assign_List := Current.Assign_List;
      end if;
      Current := new Node'(ASSIGN_NODE,
                      Redirections => Current.Redirections,
                      Assign_List => Assign_List,
                           Pos => Current.Pos);
      Deallocate (Node_Table.Table (N));
      Node_Table.Table (N) := Current;
      Append (Current.Assign_List, S);
   end Append_Assignement;

   function Get_Node (N : Node_Id) return Node_Access is
   begin
      if N = Null_Node then
         return null;
      else
         return Node_Table.Table (N);
      end if;
   end Get_Node;

   ------------------
   -- Set_Cmd_Node --
   ------------------

   procedure Set_Cmd_Node
     (N           : Node_Id;
      Cmd         : Annotated_String)
   is
      Current : Node_Access := Node_Table.Table (N);
      Assign_List : Annotated_String_List;
   begin
      if Current.Kind = ASSIGN_NODE then
         Assign_List := Current.Assign_List;
      end if;

      Current := new Node'(CMD_NODE,
                      Redirections    => Current.Redirections,
                      Arguments       => Null_Annotated_String_List,
                      Cmd             => Cmd,
                      Cmd_Assign_List => Assign_List,
                      Pos             => Current.Pos);
      Deallocate (Node_Table.Table (N));
      Node_Table.Table (N) := Current;
   end Set_Cmd_Node;

   ---------------
   -- Free_Node --
   ---------------

   procedure Free_Node (N : in out Node_Access) is
   begin
      if N = null then
         return;
      end if;

      case N.Kind is
         when IF_NODE =>
            Free_Node (N.Cond);
            Free_Node (N.True_Code);
            Free_Node (N.False_Code);
         when LIST_NODE =>
            for Index in N.List_Childs'Range loop
               Free_Node (N.List_Childs (Index));
            end loop;
         when AND_OR_LIST_NODE =>
            for Index in N.And_Or_List_Childs'Range loop
               Free_Node (N.And_Or_List_Childs (Index).N);
            end loop;
         when BRACE_NODE =>
            Free_Node (N.Brace_Code);
         when CASE_LIST_NODE =>
            Free_Node (N.Match_Code);
            Free_Node (N.Next_Patterns);
         when CASE_NODE =>
            Free_Node (N.First_Case);
         when FOR_NODE =>
            Free_Node (N.Loop_Code);
         when PIPE_NODE =>
            Free_Node (N.Pipe_Left);
            Free_Node (N.Pipe_Right);
         when SUBSHELL_NODE =>
            Free_Node (N.Subshell_Code);
         when UNTIL_NODE =>
            Free_Node (N.Until_Cond);
            Free_Node (N.Until_Code);
         when WHILE_NODE =>
            Free_Node (N.While_Cond);
            Free_Node (N.While_Code);
         when others =>
            null;
      end case;

      Deallocate (N);
   end Free_Node;

   procedure Free_Node (N : Node_Id) is
   begin
      if N = Null_Node then
         return;
      end if;
      Free_Node (Node_Table.Table (N));
      Set_Item (Node_Table, N, null);
      while Next_Node > 1 and then Node_Table.Table (Next_Node - 1) = null loop
         Next_Node := Next_Node - 1;
      end loop;

   end Free_Node;

   -----------------------
   -- Set_Function_Node --
   -----------------------

   procedure Set_Function_Node
     (N    : Node_Id;
      Name : Annotated_String;
      Function_Code : Node_Id)
   is
      Current : Node_Access := Node_Table.Table (N);
   begin
      Current := new Node'(FUNCTION_NODE,
                           Redirections => Current.Redirections,
                           Pos => Current.Pos,
                           Function_Name => Name,
                           Function_Code => Function_Code);
      Deallocate (Node_Table.Table (N));
      Node_Table.Table (N) := Current;
   end Set_Function_Node;

   ------------------
   -- Set_Node_Pos --
   ------------------

   procedure Set_Node_Pos
     (N : Node_Id;
      Pos : Text_Position)
   is
      Current : constant Node_Access := Node_Table.Table (N);
   begin
      Current.Pos := Pos;
   end Set_Node_Pos;

   --------------------------
   -- Set_Node_Redirection --
   --------------------------

   procedure Set_Node_Redirection
     (N         : Node_Id;
      Target_FD : Natural;
      Filename  : Annotated_String;
      Source_FD : Natural;
      Cmd       : Redir_Cmd)
   is
      Tmp     : constant Node_Access := Node_Table.Table (N);
      New_Top : constant Natural := Tmp.Redirections.Top + 1;
   begin
      Tmp.Redirections.Top := New_Top;
      Tmp.Redirections.Ops (New_Top) := (Target_FD, Cmd, Source_FD, Filename);
   end Set_Node_Redirection;

begin
   Init (Node_Table);
end Posix_Shell.Tree;
