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

with Sh.Buffers; use Sh.Buffers;
with Sh.List_Pools; use Sh.List_Pools;
with Sh.Tokens; use Sh.Tokens;
with GNAT.Dynamic_Tables;

package Sh.Tree is

   type Shell_Tree is private;
   type Shell_Tree_Access is access Shell_Tree;

   package Node_Id_Tables is new GNAT.Dynamic_Tables
     (Node_Id,
      Positive,
      1,
      16,
      100);

   type Node_Id_Array is array (Positive range <>) of Node_Id;

   --------------------
   -- Tree modifiers --
   --------------------

   --  The following functions are dealing with the addition or modification of
   --  nodes in a tree.

   function Add_Block_Node
     (Tree    : in out Shell_Tree;
      Code    : Node_Id)
      return Node_Id;

   function Add_If_Node
     (Tree          : in out Shell_Tree;
      Cond          : Node_Id;
      True_Code     : Node_Id;
      False_Code    : Node_Id)
      return Node_Id;

   function Add_Pipe_Node
     (Tree          : in out Shell_Tree;
      Childs        : Node_Id_Array;
      Pipe_Negation : Boolean)
      return Node_Id;

   function Add_Case_Node
     (Tree           : in out Shell_Tree;
      Case_Value     : Token;
      Case_List_Code : Node_Id)
      return Node_Id;

   function Add_Case_List_Node
     (Tree           : in out Shell_Tree;
      Pattern        : Token_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id)
      return Node_Id;

   function Add_For_Node
     (Tree               : in out Shell_Tree;
      Variable_Name      : Token;
      Value_List         : Token_List;
      Loop_Code          : Node_Id;
      Default_Value_List : Boolean)
      return Node_Id;

   function Add_Brace_Node
     (Tree       : in out Shell_Tree;
      Brace_Code : Node_Id)
      return Node_Id;

   function Add_Subshell_Node
     (Tree          : in out Shell_Tree;
      Subshell_Code : Node_Id)
      return Node_Id;

   function Add_Until_Node
     (Tree            : in out Shell_Tree;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_While_Node
     (Tree            : in out Shell_Tree;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_Null_Node (Tree : in out Shell_Tree) return Node_Id;

   procedure Set_Cmd_Node
     (Tree   : in out Shell_Tree;
      N      : Node_Id;
      Cmd    : Token);

   procedure Set_Function_Node
     (Tree          : in out Shell_Tree;
      N             : Node_Id;
      Name          : Token;
      Function_Code : in out Shell_Tree);

   procedure Set_Node_Redirection
     (Tree      : Shell_Tree;
      N         : Node_Id;
      Operator  : Redirection);

   procedure Set_Node_Continuation
     (Tree          : Shell_Tree;
      N             : Node_Id;
      Cont_If_True  : Node_Id;
      Cont_If_False : Node_Id);

   function Get_True_Continuation
     (Tree : Shell_Tree;
      N    : Node_Id)
      return Node_Id;

   function Get_False_Continuation
     (Tree : Shell_Tree;
      N    : Node_Id)
      return Node_Id;

   procedure Append_Arg
     (Tree   : in out Shell_Tree;
      N      : Node_Id;
      S      : Token);

   procedure Append_Assignement
     (Tree   : in out Shell_Tree;
      N      : Node_Id;
      S      : Token);

   procedure Set_Node_Pos
     (Tree   : Shell_Tree;
      N      : Node_Id;
      Pos    : Text_Position);

   procedure Free_Node
     (Tree   : in out Shell_Tree; N : Node_Id);

   procedure Free_Node
     (Tree   : in out Shell_Tree);

   function New_Tree
     (B            : Buffer;
      Protect      : Boolean := False;
      Allow_Return : Boolean := False)
      return Shell_Tree;
   --  Create a new tree associated with buffer B. If Protect is set to True
   --  then Tree cannot be deallocatd (a call to Protect_Tree is done)

   procedure Protect_Tree (T : in out Shell_Tree);
   --  Ensure tree cannot be deallocated and neither its associated buffer

   procedure Protect_Tree_Buffer (T : in out Shell_Tree);
   --  Ensure that the tree buffer cannot be deallocated

   procedure Allow_Return (T : in out Shell_Tree);

   function Token_List_Pool (T : Shell_Tree) return List_Pool;
   procedure Append
     (Tree : in out Shell_Tree;
      List : in out Token_List;
      T    : Token);

   procedure Set_Tree_Toplevel (Tree : in out Shell_Tree; N : Node_Id);
   type Node is private;
   type Node_Access is access Node;

   procedure Push_Pending_Here_Doc
     (Tree        : in out Shell_Tree;
      Target_Node : Node_Id;
      Target_Fd   : Integer);
   --  Add pending here document

   procedure Pop_Pending_Here_Doc
     (Tree        : in out Shell_Tree;
      Target_Node : out Node_Id;
      Target_Fd   : out Integer);
   --  Pop next pending here document

   function Has_Pending_Here_Doc (Tree : Shell_Tree) return Boolean;
   --  Return True if there are some pending here documents

private

   type Node_Kind is
     (ASSIGN_NODE,
      BLOCK_NODE,
      BRACE_NODE,
      CASE_LIST_NODE,
      CASE_NODE,
      CMD_NODE,
      FOR_NODE,
      FUNCTION_NODE,
      IF_NODE,
      NOP_NODE,
      NULL_CMD_NODE,
      PIPE_NODE,
      SUBSHELL_NODE,
      UNTIL_NODE,
      WHILE_NODE);

   type Node_Id_Array_Access is access Node_Id_Array;

   function Get_Node (Tree : Shell_Tree; N : Node_Id) return Node;

   type Node (Kind : Node_Kind := PIPE_NODE) is record
      Redirections       : Redirection_Stack := Empty_Redirections;
      Pos                : Text_Position := Null_Text_Position;
      Cont_If_True       : Node_Id := 0;
      Cont_If_False      : Node_Id := 0;

      case Kind is
         when BLOCK_NODE =>
            Code                : Node_Id;
         when BRACE_NODE =>
            Brace_Code          : Node_Id;
         when CASE_LIST_NODE =>
            Pattern_List        : Token_List;
            Match_Code          : Node_Id;
            Next_Patterns       : Node_Id;
         when CASE_NODE =>
            Case_Word           : Token;
            First_Case          : Node_Id;
         when IF_NODE =>
            If_Condition        : Node_Id;
            If_True_Code        : Node_Id;
            If_False_Code       : Node_Id;
         when FOR_NODE =>
            Loop_Var            : Token;
            Loop_Var_Values     : Token_List;
            Loop_Code           : Node_Id;
            Loop_Default_Values : Boolean;
         when NULL_CMD_NODE =>
            null;
         when PIPE_NODE =>
            Pipe_Childs         : Node_Id_Array_Access;
            Pipe_Negation       : Boolean;
         when SUBSHELL_NODE =>
            Subshell_Code       : Node_Id;
         when FUNCTION_NODE =>
            Function_Name       : Token;
            Function_Code       : Shell_Tree_Access;
         when CMD_NODE =>
            Cmd                 : Token;
            Arguments           : Token_List;
            Cmd_Assign_List     : Token_List;
         when ASSIGN_NODE =>
            Assign_List         : Token_List;
         when UNTIL_NODE =>
            Until_Cond          : Node_Id;
            Until_Code          : Node_Id;
         when WHILE_NODE =>
            While_Cond          : Node_Id;
            While_Code          : Node_Id;
         when others =>
            null;
      end case;
   end record;

   package Node_Tables is new GNAT.Dynamic_Tables
     (Node,
      Positive,
      1,
      16,
      100);

   use Node_Tables;

   type Pending_Here_Doc is record
      Target_Node : Node_Id;
      Target_Fd   : Integer;
   end record;

   type Pending_Here_Docs is array (1 .. 16) of Pending_Here_Doc;

   type Shell_Tree is record
      Node_Table     : Instance;
      Next_Node      : Node_Id := 1;
      Toplevel_Node  : Node_Id := 0;
      Pool           : List_Pool;
      Buffer         : Sh.Buffers.Buffer;
      Protect_Tree   : Boolean := False;
      Protect_Buffer : Boolean := False;
      Allow_Return   : Boolean := False;

      --  Used during parsing only
      Pending_Redirections : Pending_Here_Docs := (others => (0, 0));
      Pending_Redirections_Last : Natural := 0;
   end record;

end Sh.Tree;
