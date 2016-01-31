------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

package body Posix_Shell.Tree is

   function Add_Node
     (Tree : in out Shell_Tree;
      N    : Node)
      return Node_Id;

   procedure Free_Node (Tree : in out Shell_Tree; N : in out Node);

   procedure Deallocate is
     new Ada.Unchecked_Deallocation
       (Node_Id_Array,
        Node_Id_Array_Access);

   procedure Deallocate is
     new Ada.Unchecked_Deallocation
       (Shell_Tree,
        Shell_Tree_Access);

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

   --------------------
   -- Add_Block_Node --
   --------------------

   function Add_Block_Node
     (Tree    : in out Shell_Tree;
      Code    : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind               => BLOCK_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          Code               => Code));
   end Add_Block_Node;

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
         (Kind               => BRACE_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          Brace_Code         => Brace_Code));
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
         (Kind               => CASE_LIST_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          Pattern_List       => Pattern,
          Match_Code         => Case_Code,
          Next_Patterns      => Next_Case_Code));
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
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections => Empty_Redirections,
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
          Cont_If_True        => 0,
          Cont_If_False       => 0,
          Redirections        => Empty_Redirections,
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
     (Tree       : in out Shell_Tree;
      Cond       : Node_Id;
      True_Code  : Node_Id;
      False_Code : Node_Id)
      return Node_Id
   is
   begin
      return Add_Node
        (Tree,
         (Kind               => IF_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          If_Condition       => Cond,
          If_True_Code       => True_Code,
          If_False_Code      => False_Code));
   end Add_If_Node;

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
         (Kind         => NULL_CMD_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections => Empty_Redirections,
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
         (Kind               => PIPE_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          Pipe_Childs        => Child_List,
          Pipe_Negation      => Pipe_Negation));
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
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections  => Empty_Redirections,
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
         (Kind               => UNTIL_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          Until_Cond         => Cond,
          Until_Code         => Loop_Code));
   end Add_Until_Node;

   --------------------
   -- Add_While_Node --
   --------------------

   function Add_While_Node
     (Tree            : in out Shell_Tree;
      Cond, Loop_Code : Node_Id) return Node_Id is
   begin
      return Add_Node
        (Tree,
         (Kind               => WHILE_NODE,
          Cont_If_True       => 0,
          Cont_If_False      => 0,
          Redirections       => Empty_Redirections,
          Pos                => Null_Text_Position,
          While_Cond         => Cond,
          While_Code         => Loop_Code));
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
                  Cont_If_True       => 0,
                  Cont_If_False      => 0,
                  Redirections => Current.Redirections,
                  Assign_List  => Assign_List,
                  Pos          => Current.Pos);
      Tree.Node_Table.Table (N) := Current;
   end Append_Assignement;

   ------------------
   -- Allow_Return --
   ------------------

   procedure Allow_Return (T : in out Shell_Tree) is
   begin
      T.Allow_Return := True;
   end Allow_Return;

   --------------
   -- New_Tree --
   --------------

   function New_Tree
     (B            : Buffer;
      Protect      : Boolean := False;
      Allow_Return : Boolean := False)
      return Shell_Tree
   is
      T : Shell_Tree;
   begin
      Init (T.Node_Table);
      T.Pool := New_Pool;
      T.Buffer := B;
      if Protect then
         Protect_Tree (T);
      end if;
      T.Allow_Return := Allow_Return;
      return T;
   end New_Tree;

   ------------------
   -- Protect_Tree --
   ------------------

   procedure Protect_Tree (T : in out Shell_Tree) is
   begin
      T.Protect_Tree := True;
      T.Protect_Buffer := True;
   end Protect_Tree;

   -------------------------
   -- Protect_Tree_Buffer --
   -------------------------

   procedure Protect_Tree_Buffer (T : in out Shell_Tree) is
   begin
      T.Protect_Buffer := True;
   end Protect_Tree_Buffer;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Tree : Shell_Tree;
      N    : Node_Id) return Node is
   begin
      if N = Null_Node then
         return (NOP_NODE,
                 Cont_If_True       => 0,
                 Cont_If_False      => 0,
                 Redirections       => Empty_Redirections,
                 Pos                => Null_Text_Position);
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
         Cont_If_True       => 0,
         Cont_If_False      => 0,
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

   procedure Free_Node (Tree : in out Shell_Tree; N : in out Node) is
   begin
      case N.Kind is
         when BRACE_NODE =>
            Free_Node (Tree, N.Brace_Code);
         when CASE_LIST_NODE =>
            Deallocate (Tree.Pool, N.Pattern_List);
            Free_Node (Tree, N.Match_Code);
            Free_Node (Tree, N.Next_Patterns);
         when CASE_NODE =>
            Free_Node (Tree, N.First_Case);
         when FOR_NODE =>
            Free_Node (Tree, N.Loop_Code);
         when PIPE_NODE =>
            for Index in N.Pipe_Childs'Range loop
               Free_Node (Tree, N.Pipe_Childs (Index));
            end loop;
            Deallocate (N.Pipe_Childs);
         when SUBSHELL_NODE =>
            Free_Node (Tree, N.Subshell_Code);
         when FUNCTION_NODE =>
            Deallocate (N.Function_Code);
         when BLOCK_NODE =>
            Free_Node (Tree, N.Code);
         when others =>
            null;
      end case;
   end Free_Node;

   ---------------
   -- Free_Node --
   ---------------

   procedure Free_Node
     (Tree   : in out Shell_Tree) is
   begin
      if not Tree.Protect_Tree then
         Free_Node (Tree, Tree.Toplevel_Node);
         Free (Tree.Node_Table);
         Deallocate (Tree.Pool);
         if not Tree.Protect_Buffer then
            Deallocate (Tree.Buffer);
         end if;
      end if;
   end Free_Node;

   procedure Free_Node (Tree : in out Shell_Tree; N : Node_Id) is
   begin
      if N = Null_Node then
         return;
      end if;
      Free_Node (Tree, Tree.Node_Table.Table (N));
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
         Cont_If_True       => 0,
         Cont_If_False      => 0,
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
      Operator  : Redirection)
   is

   begin
      Push (Tree.Node_Table.Table (N).Redirections, Operator);
   end Set_Node_Redirection;

   ---------------------------
   -- Set_Node_Continuation --
   ---------------------------

   procedure Set_Node_Continuation
     (Tree          : Shell_Tree;
      N             : Node_Id;
      Cont_If_True  : Node_Id;
      Cont_If_False : Node_Id)
   is
   begin
      if Cont_If_True /= 0 then
         Tree.Node_Table.Table (N).Cont_If_True := Cont_If_True;
      end if;
      if Cont_If_False /= 0 then
         Tree.Node_Table.Table (N).Cont_If_False := Cont_If_False;
      end if;

   end Set_Node_Continuation;

   function Get_True_Continuation
     (Tree : Shell_Tree;
      N    : Node_Id)
      return Node_Id
   is
   begin
      return Tree.Node_Table.Table (N).Cont_If_True;
   end Get_True_Continuation;

   function Get_False_Continuation
     (Tree : Shell_Tree;
      N    : Node_Id)
      return Node_Id
   is
   begin
      return Tree.Node_Table.Table (N).Cont_If_False;
   end Get_False_Continuation;

   -----------------------
   -- Set_Tree_Toplevel --
   -----------------------

   procedure Set_Tree_Toplevel (Tree : in out Shell_Tree; N : Node_Id) is
   begin
      Tree.Toplevel_Node := N;
   end Set_Tree_Toplevel;

end Posix_Shell.Tree;
