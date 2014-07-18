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

with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Buffers; use Posix_Shell.Buffers;
with Posix_Shell.List_Pools; use Posix_Shell.List_Pools;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;
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
     (Tree   : in out Shell_Tree;
      Childs : Node_Id_Array)
      return Node_Id;
   --  Create a node that represent a list of ; or newline separated statements
   --  Childs is the list of nodes that goes into that sequence.

   function Add_And_Or_List_Node
     (Tree   : in out Shell_Tree;
      Childs : And_Or_Node_Id_Array)
      return Node_Id;
   --  Create a node that represent a list of statements separated by || or &&

   function Add_Pipe_Node
     (Tree          : in out Shell_Tree;
      Childs        : Node_Id_Array;
      Pipe_Negation : Boolean)
      return Node_Id;

   function Add_Until_Node
     (Tree            : in out Shell_Tree;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_While_Node
     (Tree            : in out Shell_Tree;
      Cond, Loop_Code : Node_Id)
      return Node_Id;

   function Add_If_Node
     (Tree       : in out Shell_Tree;
      Cond       : Node_Id;
      True_Code  : Node_Id;
      False_Code : Node_Id)
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
      Target_FD : Natural;
      Filename  : Token;
      Source_FD : Natural;
      Cmd       : Redir_Cmd;
      Eval      : Boolean);

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

   function New_Tree (B : Buffer) return Shell_Tree;

   function Token_List_Pool (T : Shell_Tree) return List_Pool;
   procedure Append
     (Tree : in out Shell_Tree;
      List : in out Token_List;
      T    : Token);

   procedure Set_Tree_Toplevel (Tree : in out Shell_Tree; N : Node_Id);
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
      NULL_CMD_NODE,
      NOP_NODE);

   type And_Or_Record is record
      N    : Node_Access;
      Kind : List_Kind;
   end record;

   type Node_Id_Array_Access is access Node_Id_Array;
   type And_Or_Node_Id_Array_Access is access And_Or_Node_Id_Array;



   function Get_Node (Tree : Shell_Tree; N : Node_Id) return Node;

   type Node (Kind : Node_Kind := IF_NODE) is record
      Redirections      : Redirection_Op_Stack := Empty_Redirection_Op_Stack;
      Pos               : Text_Position := Null_Text_Position;
      case Kind is
         when IF_NODE =>
            Cond               : Node_Id;
            True_Code          : Node_Id;
            False_Code         : Node_Id;
         when LIST_NODE =>
            List_Childs        : Node_Id_Array_Access;
         when AND_OR_LIST_NODE =>
            And_Or_List_Childs : And_Or_Node_Id_Array_Access;
         when BRACE_NODE =>
            Brace_Code         : Node_Id;
         when CASE_LIST_NODE =>
            Pattern_List       : Token_List;
            Match_Code         : Node_Id;
            Next_Patterns      : Node_Id;
         when CASE_NODE =>
            Case_Word          : Token;
            First_Case         : Node_Id;
         when FOR_NODE =>
            Loop_Var            : Token;
            Loop_Var_Values     : Token_List;
            Loop_Code           : Node_Id;
            Loop_Default_Values : Boolean;
         when NULL_CMD_NODE =>
            null;
         when PIPE_NODE =>
            Pipe_Childs        : Node_Id_Array_Access;
            Pipe_Negation      : Boolean;
         when SUBSHELL_NODE =>
            Subshell_Code      : Node_Id;
         when UNTIL_NODE =>
            Until_Cond         : Node_Id;
            Until_Code         : Node_Id;
         when WHILE_NODE =>
            While_Cond         : Node_Id;
            While_Code         : Node_Id;
         when FUNCTION_NODE =>
            Function_Name      : Token;
            Function_Code      : Shell_Tree_Access;
         when CMD_NODE =>
            Cmd                : Token;
            Arguments          : Token_List;
            Cmd_Assign_List    : Token_List;
         when ASSIGN_NODE =>
            Assign_List         : Token_List;
         when others =>
            null;
      end case;
   end record;

   package Node_Tables is new GNAT.Dynamic_Tables
     (Node,
      Positive,
      1,
      4,
      100);

   use Node_Tables;

   type Shell_Tree is record
      Node_Table    : Instance;
      Next_Node     : Node_Id := 1;
      Toplevel_Node : Node_Id := 0;
      Pool          : List_Pool;
      Buffer        : Posix_Shell.Buffers.Buffer;
   end record;
end Posix_Shell.Tree;
