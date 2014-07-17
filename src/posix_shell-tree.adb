------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2014, AdaCore                   --
--                                                                          --
-- GSH is free software;  you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GSH is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GSH is maintained by AdaCore (http://www.adacore.com)                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
--  with Ada.Text_IO;

package body Posix_Shell.Tree is

   function Add_Node
     (Tree : in out Shell_Tree;
      N    : Node)
      return Node_Id;

   procedure Free_Node (Tree : in out Shell_Tree; N : in out Node_Access);

   procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Node,
           Node_Access);

   function Token_List_Pool (T : Shell_Tree) return List_Pool
   is
   begin
      return T.Pool;
   end Token_List_Pool;

   procedure Append
     (Tree : in out Shell_Tree;
      List : in out Token_List;
      T    : Token)
   is
   begin
      Append (Tree.Pool, List, T);
   end Append;

   -----------------------
   -- Add_And_Or_List_Node --
   -----------------------

   function Add_And_Or_List_Node
     (Tree   : in out Shell_Tree;
      Childs : And_Or_Node_Id_Array)
      return Node_Id
   is
      Child_List : constant And_Or_Node_Id_Array_Access :=
        new And_Or_Node_Id_Array'(Childs);
   begin
      return Add_Node
        (Tree,
         (Kind => AND_OR_LIST_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          And_Or_List_Childs => Child_List));
   end Add_And_Or_List_Node;

   --------------------
   -- Add_Brace_Node --
   --------------------

   function Add_Brace_Node
     (Tree       : in out Shell_Tree;
      Brace_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind => BRACE_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          Brace_Code   => Brace_Code));
   end Add_Brace_Node;

   ------------------------
   -- Add_Case_List_Node --
   ------------------------

   function Add_Case_List_Node
     (Tree           : in out Shell_Tree;
      Pattern        : Token_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind => CASE_LIST_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          Pattern_List => Pattern,
          Match_Code   => Case_Code,
          Next_Patterns => Next_Case_Code));
   end Add_Case_List_Node;

   -------------------
   -- Add_Case_Node --
   -------------------

   function Add_Case_Node
     (Tree           : in out Shell_Tree;
      Case_Value     : Token;
      Case_List_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind => CASE_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          Case_Word    => Case_Value,
          First_Case   => Case_List_Code));
   end Add_Case_Node;

   ------------------
   -- Add_For_Node --
   ------------------

   function Add_For_Node
     (Tree               : in out Shell_Tree;
      Variable_Name      : Token;
      Value_List         : Token_List;
      Loop_Code          : Node_Id;
      Default_Value_List : Boolean)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind                => FOR_NODE,
          Redirections        => Empty_Redirection_Op_Stack,
          Pos                 => Null_Text_Position,
          Loop_Var            => Variable_Name,
          Loop_Var_Values     => Value_List,
          Loop_Code           => Loop_Code,
          Loop_Default_Values => Default_Value_List));
   end Add_For_Node;

   -----------------
   -- Add_If_Node --
   -----------------

   function Add_If_Node
     (Tree                        : in out Shell_Tree;
      Cond, True_Code, False_Code : Node_Id) return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind => IF_NODE,
          Cond => Cond,
          True_Code => True_Code,
          False_Code => False_Code,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position));

   end Add_If_Node;

   -------------------
   -- Add_List_Node --
   -------------------

   function Add_List_Node
     (Tree   : in out Shell_Tree;
      Childs : Node_Id_Array)
      return Node_Id
   is
      Child_List : constant Node_Id_Array_Access := new Node_Id_Array'(Childs);
   begin
      return Add_Node
        (Tree,
         (Kind => LIST_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          List_Childs  => Child_List));
   end Add_List_Node;

   --------------
   -- Add_Node --
   --------------

   function Add_Node
     (Tree   : in out Shell_Tree;
      N      : Node)
      return Node_Id
   is
   begin
      Set_Item (Tree.Node_Table, Tree.Next_Node, N);
      Tree.Next_Node := Tree.Next_Node + 1;
      return Tree.Next_Node - 1;
   end Add_Node;

   -------------------
   -- Add_Null_Node --
   -------------------

   function Add_Null_Node (Tree : in out Shell_Tree) return Node_Id is
   begin
      return Add_Node
        (Tree,
         (Kind => NULL_CMD_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position));
   end Add_Null_Node;

   -------------------
   -- Add_Pipe_Node --
   -------------------

   function Add_Pipe_Node
     (Tree          : in out Shell_Tree;
      Childs        : Node_Id_Array;
      Pipe_Negation : Boolean)
      return Node_Id
   is
      Child_List : constant Node_Id_Array_Access := new Node_Id_Array'(Childs);
   begin
      return Add_Node
        (Tree,
         (Kind => PIPE_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          Pipe_Childs  => Child_List,
          Pipe_Negation => Pipe_Negation));
   end Add_Pipe_Node;

   -----------------------
   -- Add_Subshell_Node --
   -----------------------

   function Add_Subshell_Node
     (Tree          : in out Shell_Tree;
      Subshell_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind          => SUBSHELL_NODE,
          Redirections  => Empty_Redirection_Op_Stack,
          Pos           => Null_Text_Position,
          Subshell_Code => Subshell_Code));
   end Add_Subshell_Node;

   --------------------
   -- Add_Until_Node --
   --------------------

   function Add_Until_Node
     (Tree   : in out Shell_Tree;
      Cond, Loop_Code : Node_Id) return Node_Id is
   begin
      return Add_Node
        (Tree,
         (Kind         => UNTIL_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          Until_Cond   => Cond,
          Until_Code   => Loop_Code));
   end Add_Until_Node;

   --------------------
   -- Add_While_Node --
   --------------------

   function Add_While_Node
     (Tree   : in out Shell_Tree;
      Cond, Loop_Code : Node_Id) return Node_Id is
   begin
      return Add_Node
        (Tree,
         (Kind => WHILE_NODE,
          Redirections => Empty_Redirection_Op_Stack,
          Pos          => Null_Text_Position,
          While_Cond   => Cond,
          While_Code   => Loop_Code));
   end Add_While_Node;

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg
     (Tree : in out Shell_Tree;
      N    : Node_Id;
      S    : Token)
   is
   begin
      Append (Tree, Tree.Node_Table.Table (N).Arguments, S);
   end Append_Arg;

   ------------------------
   -- Append_Assignement --
   ------------------------

   procedure Append_Assignement
     (Tree   : in out Shell_Tree;
      N      : Node_Id;
      S      : Token)
   is
      Current     : Node := Tree.Node_Table.Table (N);
      Assign_List : Token_List := Null_List;

   begin
      if Current.Kind /= NULL_CMD_NODE then
         Assign_List := Current.Assign_List;
      end if;
      Append (Tree, Assign_List, S);
      Current := (ASSIGN_NODE,
                  Redirections => Current.Redirections,
                  Assign_List  => Assign_List,
                  Pos          => Current.Pos);
      Tree.Node_Table.Table (N) := Current;
   end Append_Assignement;

   function New_Tree return Shell_Tree is
      T : Shell_Tree;
   begin
      Init (T.Node_Table);
      T.Pool := New_Pool;
      return T;
   end New_Tree;

   function Get_Node
     (Tree : Shell_Tree;
      N    : Node_Id) return Node is
   begin
      if N = Null_Node then
         return (NOP_NODE, Empty_Redirection_Op_Stack,
                 Null_Text_Position);
      else
         return Tree.Node_Table.Table (N);
      end if;
   end Get_Node;

   ------------------
   -- Set_Cmd_Node --
   ------------------

   procedure Set_Cmd_Node
     (Tree        : in out Shell_Tree;
      N           : Node_Id;
      Cmd         : Token)
   is
      Current : Node := Tree.Node_Table.Table (N);
      Assign_List : Token_List := Null_List;
   begin
      if Current.Kind = ASSIGN_NODE then
         Assign_List := Current.Assign_List;
      end if;

      Current :=
        (CMD_NODE,
         Redirections    => Current.Redirections,
         Arguments       => Null_List,
         Cmd             => Cmd,
         Cmd_Assign_List => Assign_List,
         Pos             => Current.Pos);
      Tree.Node_Table.Table (N) := Current;
   end Set_Cmd_Node;

   ---------------
   -- Free_Node --
   ---------------

   procedure Free_Node (Tree : in out Shell_Tree; N : in out Node_Access) is
   begin
      if N = null then
         return;
      end if;

      case N.Kind is
         when IF_NODE =>
            Free_Node (Tree, N.Cond);
            Free_Node (Tree, N.True_Code);
            Free_Node (Tree, N.False_Code);
         when LIST_NODE =>
            for Index in N.List_Childs'Range loop
               Free_Node (Tree, N.List_Childs (Index));
            end loop;
         when AND_OR_LIST_NODE =>
            for Index in N.And_Or_List_Childs'Range loop
               Free_Node (Tree, N.And_Or_List_Childs (Index).N);
            end loop;
         when BRACE_NODE =>
            Free_Node (Tree, N.Brace_Code);
         when CASE_LIST_NODE =>
            Deallocate (Tree.Pool, N.Pattern_List);
            Free_Node (Tree, N.Match_Code);
            Free_Node (Tree, N.Next_Patterns);
         when CASE_NODE =>
            Free_Node (Tree, N.First_Case);
         when FOR_NODE =>
            Deallocate (Tree.Pool, N.Loop_Var_Values);
            Free_Node (Tree, N.Loop_Code);
         when PIPE_NODE =>
            for Index in N.Pipe_Childs'Range loop
               Free_Node (Tree, N.Pipe_Childs (Index));
            end loop;
         when SUBSHELL_NODE =>
            Free_Node (Tree, N.Subshell_Code);
         when UNTIL_NODE =>
            Free_Node (Tree, N.Until_Cond);
            Free_Node (Tree, N.Until_Code);
         when WHILE_NODE =>
            Free_Node (Tree, N.While_Cond);
            Free_Node (Tree, N.While_Code);
         when CMD_NODE =>
            Deallocate (Tree.Pool, N.Arguments);
            Deallocate (Tree.Pool, N.Cmd_Assign_List);
         when ASSIGN_NODE =>
            Deallocate (Tree.Pool, N.Assign_List);
         when others =>
            null;
      end case;

      Deallocate (N);
   end Free_Node;

   ---------------
   -- Free_Node --
   ---------------

   procedure Free_Node
     (Tree   : in out Shell_Tree) is
   begin
      Free_Node (Tree, Tree.Toplevel_Node);
      Free (Tree.Node_Table);
   end Free_Node;

   procedure Free_Node (Tree : in out Shell_Tree; N : Node_Id) is
   begin
      if N = Null_Node then
         return;
      end if;
      --  ???? Free_Node (Tree, Tree.Node_Table.Table (N));
   end Free_Node;

   -----------------------
   -- Set_Function_Node --
   -----------------------

   procedure Set_Function_Node
     (Tree          : in out Shell_Tree;
      N             : Node_Id;
      Name          : Token;
      Function_Code : in out Shell_Tree)
   is
      Current : Node := Tree.Node_Table.Table (N);
   begin
      Current :=
        (FUNCTION_NODE,
         Redirections => Current.Redirections,
         Pos          => Current.Pos,
         Function_Name => Name,
         Function_Code => new Shell_Tree'(Function_Code));
      Tree.Node_Table.Table (N) := Current;
   end Set_Function_Node;

   ------------------
   -- Set_Node_Pos --
   ------------------

   procedure Set_Node_Pos
     (Tree : Shell_Tree;
      N    : Node_Id;
      Pos  : Text_Position)
   is

   begin
      Tree.Node_Table.Table (N).Pos := Pos;
   end Set_Node_Pos;

   --------------------------
   -- Set_Node_Redirection --
   --------------------------

   procedure Set_Node_Redirection
     (Tree      : Shell_Tree;
      N         : Node_Id;
      Target_FD : Natural;
      Filename  : Token;
      Source_FD : Natural;
      Cmd       : Redir_Cmd;
      Eval      : Boolean)
   is
      New_Top : constant Natural :=
        Tree.Node_Table.Table (N).Redirections.Top + 1;
   begin
      Tree.Node_Table.Table (N).Redirections.Top := New_Top;
      Tree.Node_Table.Table (N).Redirections.Ops (New_Top) :=
        (Target_FD, Cmd, Source_FD, Filename, Eval);
   end Set_Node_Redirection;

   -----------------------
   -- Set_Tree_Toplevel --
   -----------------------

   procedure Set_Tree_Toplevel (Tree : in out Shell_Tree; N : Node_Id) is
   begin
      Tree.Toplevel_Node := N;
   end Set_Tree_Toplevel;

end Posix_Shell.Tree;
