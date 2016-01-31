------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                            Sh.Parser                            --
--                                                                          --
--                                 B o d y                                  --
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

with Sh.List_Pools; use Sh.List_Pools;
with Sh.Traces; use Sh.Traces;

package body Sh.Parser is

   type Parser_Context is
     (NULL_CONTEXT,
      IF_COND_CONTEXT,
      IF_THEN_CONTEXT,
      IF_ELSE_CONTEXT,
      SUBSHELL_CONTEXT,
      CASE_ITEM_CONTEXT,
      LOOP_COND_CONTEXT,
      DO_GROUP_CONTEXT,
      BRACEGROUP_CONTEXT);

   pragma Warnings (Off);
   type IO_Here_Context is record
      N         : Node_Id;
      Marker    : Token;
      Target_Fd : Integer;
   end record;
   pragma Warnings (On);

   type IO_Here_Context_Array is array (1 .. 8) of IO_Here_Context;
   Pending_IO_Heres      : IO_Here_Context_Array;
   Pending_IO_Heres_Last : Natural := 0;

   function Parse
     (B           : in out Token_Buffer;
      T           : in out Shell_Tree;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  Parse content of buffer B and return the toplevel Node

   --  name             : NAME                     /* Apply rule 5 */
   --                   ;
   --  in               : In                       /* Apply rule 6 */
   --                   ;

   --  case_item_ns     :     pattern ')'               linebreak
   --                   |     pattern ')' compound_list linebreak
   --                   | '(' pattern ')'               linebreak
   --                   | '(' pattern ')' compound_list linebreak
   --                   ;
   --  case_item        :     pattern ')' linebreak     DSEMI linebreak
   --                   |     pattern ')' compound_list DSEMI linebreak
   --                   | '(' pattern ')' linebreak     DSEMI linebreak
   --                   | '(' pattern ')' compound_list DSEMI linebreak
   --                   ;

   --  cmd_name         : WORD                   /* Apply rule 7a */
   --                   ;
   --  cmd_word         : WORD                   /* Apply rule 7b */
   --                   ;
   --  cmd_prefix       :            io_redirect
   --                   | cmd_prefix io_redirect
   --                   |            ASSIGNMENT_WORD
   --                   | cmd_prefix ASSIGNMENT_WORD
   --                   ;

   --  filename         : WORD                      /* Apply rule 2 */
   --                   ;
   --  io_here          : DLESS     here_end
   --                   | DLESSDASH here_end
   --                   ;
   --  here_end         : WORD                      /* Apply rule 3 */
   --                   ;

   function Parse_List
     (B           : in out Token_Buffer;
      Tree        : in out Shell_Tree;
      Context     : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  list : list separator_op and_or
   --       |                   and_or
   --       ;

   function Parse_And_Or
     (B           : in out Token_Buffer;
      T           : in out Shell_Tree;
      C           : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  and_or :                         pipeline
   --         | and_or AND_IF linebreak pipeline
   --         | and_or OR_IF  linebreak pipeline
   --         ;

   function Parse_Pipeline
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  pipeline :      pipe_sequence
   --           | Bang pipe_sequence
   --           ;

   function Parse_Pipe_Sequence
     (B             : in out Token_Buffer;
      T             : in out Shell_Tree;
      C             : Parser_Context;
      Pipe_Negation : Boolean;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  pipe_sequence :                             command
   --                | pipe_sequence '|' linebreak command
   --                ;

   function Parse_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id;
   --  command          : simple_command
   --                   | compound_command
   --                   | compound_command redirect_list
   --                   | function_definition
   --                   ;

   function Parse_Compound_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  compound_command : brace_group
   --                   | subshell
   --                   | for_clause
   --                   | case_clause
   --                   | if_clause
   --                   | while_clause
   --                   | until_clause
   --                   ;

   function Parse_Subshell
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  subshell         : '(' compound_list ')'
   --                   ;

   function Parse_Compound_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  compound_list    :              term
   --                   | newline_list term
   --                   |              term separator
   --                   | newline_list term separator
   --                   ;

   function Parse_Term
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  term             : term separator and_or
   --                   |                and_or
   --                   ;

   function Parse_For_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  for_clause      : For name linebreak                            do_group
   --                  | For name linebreak in          sequential_sep do_group
   --                  | For name linebreak in wordlist sequential_sep do_group
   --                  ;

   function Parse_Word_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Token_List;
   --  wordlist         : wordlist WORD
   --                   |          WORD
   --                   ;

   function Parse_Case_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  case_clause      : Case WORD linebreak in linebreak case_list    Esac
   --                   | Case WORD linebreak in linebreak case_list_ns Esac
   --                   | Case WORD linebreak in linebreak              Esac
   --                   ;

   function Parse_Case_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  case_list_ns     : case_list case_item_ns
   --                   |           case_item_ns
   --                   ;
   --  case_list        : case_list case_item
   --                   |           case_item
   --                   ;

   function Parse_Pattern
     (B : in out Token_Buffer;
      T : in out Shell_Tree)
     return Token_List;
   --  pattern          :             WORD         /* Apply rule 4 */
   --                   | pattern '|' WORD         /* Do not apply rule 4 */
   --                   ;

   function Parse_If_Clause
     (B         : in out Token_Buffer;
      T         : in out Shell_Tree;
      C         : Parser_Context;
      Elif_Mode : Boolean := False)
      return Node_Id;
   --  if_clause        : If compound_list Then compound_list else_part Fi
   --                   | If compound_list Then compound_list           Fi
   --                   ;

   function Parse_Else_Part
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  else_part        : Elif compound_list Then else_part
   --                   | Else compound_list
   --                   ;

   function Parse_While_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  while_clause     : While compound_list do_group
   --                   ;

   function Parse_Until_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  until_clause  : Until compound_list do_group
   --                ;

   function Parse_Brace_Group
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  brace_group      : Lbrace compound_list Rbrace
   --                   ;

   function Parse_Do_Group
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  do_group         : Do compound_list Done           /* Apply rule 6 */
   --                   ;

   function Parse_Simple_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id;
   --  simple_command   : cmd_prefix cmd_word cmd_suffix
   --                   | cmd_prefix cmd_word
   --                   | cmd_prefix
   --                   | cmd_name cmd_suffix
   --                   | cmd_name
   --                   ;

   --  cmd_suffix       :            io_redirect
   --                   | cmd_suffix io_redirect
   --                   |            WORD
   --                   | cmd_suffix WORD
   --                   ;

   procedure Parse_Redirect_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      N : Node_Id);
   --  redirect_list    :               io_redirect
   --                   | redirect_list io_redirect
   --                   ;

   procedure Parse_IO_Redirect
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      N : Node_Id);
   --  io_redirect      :           io_file
   --                   | IO_NUMBER io_file
   --                   |           io_here
   --                   | IO_NUMBER io_here
   --                   ;

   procedure Parse_Linebreak
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context);
   pragma Inline (Parse_Linebreak);
   --  newline_list     :              NEWLINE
   --                   | newline_list NEWLINE
   --                   ;
   --  linebreak        : newline_list
   --                   | /* empty */
   --                   ;

   procedure Parse_Separator
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context);
   pragma Inline (Parse_Separator);
   --  separator_op     : '&'
   --                   | ';'
   --                   ;
   --  separator        : separator_op linebreak
   --                   | newline_list
   --                   ;

   procedure Parse_Sequential_Sep
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context);

   pragma Inline (Parse_Sequential_Sep);
   --  sequential_sep   : ';' linebreak
   --                   | newline_list
   --                   ;

   procedure Parse_IO_File
     (B         : in out Token_Buffer;
      T         : in out Shell_Tree;
      C         : Parser_Context;
      N         : Node_Id;
      IO_Number : Integer);
   --  io_file          : '<'       filename
   --                   | LESSAND   filename
   --                   | '>'       filename
   --                   | GREATAND  filename
   --                   | DGREAT    filename
   --                   | LESSGREAT filename
   --                   | CLOBBER   filename
   --                   ;

   --  This is parsed by Parse_Simple_Command (otherwise need a lookahead of 2)
   --  function_definition : fname '(' ')' linebreak function_body
   --                   ;
   --  function_body    : compound_command                /* Apply rule 9 */
   --                   | compound_command redirect_list  /* Apply rule 9 */
   --                   ;
   --  fname            : NAME                            /* Apply rule 8 */
   --                   ;

   function Is_Redirection_Op
     (T           : Token_Type;
      Include_NIO : Boolean := False)
      return Boolean;
   --  Return true if T is a redirection operator.

   function Is_Input_Redirection_Op (T : Token_Type) return Boolean;
   pragma Inline (Is_Input_Redirection_Op);
   --  return True if the Token type is an Input redirector and false otherwise

   -----------------------------
   -- Is_Input_Redirection_Op --
   -----------------------------

   function Is_Input_Redirection_Op (T : Token_Type) return Boolean is
   begin
      return T in Input_Redirection_Ops'Range;
   end Is_Input_Redirection_Op;

   -----------------------
   -- Is_Redirection_Op --
   -----------------------

   function Is_Redirection_Op
     (T           : Token_Type;
      Include_NIO : Boolean := False)
      return Boolean is
   begin
      return T in Redirection_Ops
        or else (Include_NIO and then T = T_IO_NUMBER);
   end Is_Redirection_Op;

   -----------
   -- Parse --
   -----------

   function Parse
     (B           : in out Token_Buffer;
      T           : in out Shell_Tree;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is
   begin
      while Lookahead (B) = T_NEWLINE loop
         Skip_Token (B);
      end loop;
      return Parse_List (B, T, NULL_CONTEXT, Until_Token => Until_Token);
   end Parse;

   ------------------
   -- Parse_And_Or --
   ------------------

   function Parse_And_Or
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is

      use Node_Id_Tables;

      Childs : Instance;
      Child_Index : Integer := 1;
      Current     : Node_Id := Null_Node;
      Code        : Node_Id := Null_Node;
      Result : Node_Id;
      Current_Kind : Token_Type;
   begin
      Init (Childs);

      Code := Parse_Pipeline (B, T, C, Until_Token => Until_Token);
      Set_Item (Childs, Child_Index, Code);
      Child_Index := Child_Index + 1;

      loop
         Current_Kind := Lookahead (B);
         case Current_Kind is
            when T_OR_IF | T_AND_IF =>
               Skip_Token (B);
               Parse_Linebreak (B, T, C);

               Current := Parse_Pipeline (B, T, C, Until_Token => Until_Token);
               if Current = 0 then
                  exit;
               end if;

               if Current_Kind = T_OR_IF then
                  for J in reverse 1 .. Child_Index - 1 loop
                     exit when
                       Get_False_Continuation (T, Childs.Table (J)) /= 0;
                     Set_Node_Continuation (T, Childs.Table (J), 0, Current);
                  end loop;
                  Set_Item (Childs, Child_Index, Current);
               else
                  for J in reverse 1 .. Child_Index - 1 loop
                     exit when
                       Get_True_Continuation (T, Childs.Table (J)) /= 0;
                     Set_Node_Continuation (T, Childs.Table (J), Current, 0);
                  end loop;
                  Set_Item (Childs, Child_Index, Current);
               end if;
               Child_Index := Child_Index + 1;
               pragma Assert (Current /= Null_Node);
            when others =>
               exit;
         end case;
      end loop;

      if Last (Childs) = 1 then
         Result := Code;
      else
         Result := Add_Block_Node (T, Code);
      end if;

      Free (Childs);
      return Result;
   end Parse_And_Or;

   -----------------------
   -- Parse_Brace_Group --
   -----------------------

   function Parse_Brace_Group
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      pragma Unreferenced (C);
      Brace_Code : Node_Id := Null_Node;
   begin
      pragma Assert (Lookahead_Command (B) = T_LBRACE);
      Skip_Token (B);
      Brace_Code := Parse_Compound_List (B, T, BRACEGROUP_CONTEXT);
      Expect_Token (B, T_RBRACE);
      return Add_Brace_Node (T, Brace_Code);
   end Parse_Brace_Group;

   ------------------
   -- Parse_Buffer --
   ------------------

   function Parse_Buffer
     (B           : in out Token_Buffer;
      Until_Token : Token_Type := T_NULL)
      return Shell_Tree
   is
      T : Shell_Tree := New_Tree (B.B);
      N : Node_Id := 0;
   begin
      N := Parse (B, T, Until_Token => Until_Token);
      Set_Tree_Toplevel (T, N);
      return T;
   end Parse_Buffer;

   -----------------------
   -- Parse_Case_Clause --
   -----------------------

   function Parse_Case_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Case_Value : Token;
      Case_List_Code : Node_Id := Null_Node;
   begin
      pragma Assert (Lookahead_Command (B) = T_CASE);
      Skip_Token (B);
      Case_Value := Read_Word_Token (B);

      Parse_Linebreak (B, T, C);
      Expect_Token (B, T_IN);
      Parse_Linebreak (B, T, C);
      Case_List_Code := Parse_Case_List (B, T, C);
      Expect_Token (B, T_ESAC);
      return Add_Case_Node (T, Case_Value, Case_List_Code);
   end Parse_Case_Clause;

   ---------------------
   -- Parse_Case_List --
   ---------------------

   function Parse_Case_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Pattern        : Token_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id;
   begin
      if Lookahead_Command (B) = T_ESAC or else
        Lookahead_Command (B) = T_EOF
      then
         return Null_Node;
      end if;

      Pattern  := Parse_Pattern (B, T);

      Expect_Token (B, T_RPAR);
      Parse_Linebreak (B, T, C);
      if Lookahead (B) = T_DSEMI then
         Skip_Token (B);
         Parse_Linebreak (B, T, C);
         Case_Code := Null_Node;
         Next_Case_Code := Parse_Case_List (B, T, C);
      else
         Case_Code := Parse_Compound_List (B, T, CASE_ITEM_CONTEXT);
         if Lookahead_Command (B) = T_DSEMI then
            Skip_Token (B);
            Parse_Linebreak (B, T, C);
            Next_Case_Code := Parse_Case_List (B, T, C);
         else
            Next_Case_Code := Null_Node;
         end if;
      end if;
      return Add_Case_List_Node (T, Pattern, Case_Code, Next_Case_Code);
   end Parse_Case_List;

   -------------------
   -- Parse_Command --
   -------------------

   function Parse_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is
      Result : Node_Id := 0;

      function Context_Syntax_Error (Expected : Parser_Context) return Boolean;

      function Context_Image (C : Parser_Context) return String;

      -------------------
      -- Context_Image --
      -------------------

      function Context_Image (C : Parser_Context) return String is
      begin
         case C is
            when NULL_CONTEXT       => return "";
            when IF_COND_CONTEXT    => return "expect 'then'";
            when IF_THEN_CONTEXT    => return "expect 'else', 'elif' or 'fi'";
            when IF_ELSE_CONTEXT    => return "expect 'fi'";
            when SUBSHELL_CONTEXT   => return "expect ')'";
            when CASE_ITEM_CONTEXT  => return "expect 'esac' or ';;'";
            when LOOP_COND_CONTEXT  => return "expect 'do'";
            when DO_GROUP_CONTEXT   => return "expect 'done'";
            when BRACEGROUP_CONTEXT => return "expect '}'";
         end case;
      end Context_Image;

      --------------------------
      -- Context_Syntax_Error --
      --------------------------

      function Context_Syntax_Error (Expected : Parser_Context) return Boolean
      is
      begin
         if C /= Expected then
            if Until_Token = Lookahead (B) then
               return True;
            end if;
            Syntax_Error (Read_Token (B), Context_Image (C));
         end if;
         return False;
      end Context_Syntax_Error;

   begin
      case Lookahead_Command (B) is
         when T_UNTIL | T_WHILE | T_IF | T_CASE | T_FOR | T_LPAR | T_LBRACE =>
            Result := Parse_Compound_Command (B, T, C);
         when T_WORD | T_ASSIGNEMENT | T_GREAT | T_GREATAND | T_DGREAT |
              T_CLOBBER | T_LESS | T_LESSAND | T_LESSGREAT | T_DLESS |
              T_IO_NUMBER =>
            Result := Parse_Simple_Command (B, T, C);
         when T_THEN   =>
            if Context_Syntax_Error (IF_COND_CONTEXT) then
               return 0;
            end if;
         when T_DONE   =>
            if Context_Syntax_Error (DO_GROUP_CONTEXT) then
               return 0;
            end if;
         when T_DO     =>
            if Context_Syntax_Error (LOOP_COND_CONTEXT) then
               return 0;
            end if;
         when T_RPAR   =>
            if Context_Syntax_Error (SUBSHELL_CONTEXT) then
               return 0;
            end if;

         when T_DSEMI  =>
            if Context_Syntax_Error (CASE_ITEM_CONTEXT) then
               return 0;
            end if;

         when T_ESAC   =>
            if Context_Syntax_Error (CASE_ITEM_CONTEXT) then
               return 0;
            end if;
         when T_RBRACE =>
            if Context_Syntax_Error (BRACEGROUP_CONTEXT) then
               return 0;
            end if;
         when T_ELSE | T_ELIF | T_FI =>
            if C /= IF_ELSE_CONTEXT and C /= IF_THEN_CONTEXT then
               Syntax_Error (Read_Token (B), "non expected 'else'");
            end if;
         when T_EOF    => return 0;
         when others   =>
            Syntax_Error (Read_Token (B), "unexpected token ...");
      end case;
      return Result;
   end Parse_Command;

   ----------------------------
   -- Parse_Compound_Command --
   ----------------------------

   function Parse_Compound_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Result : Node_Id := 0;
   begin
      case Lookahead_Command (B) is
         when T_UNTIL  => Result := Parse_Until_Clause (B, T, C);
         when T_WHILE  => Result := Parse_While_Clause (B, T, C);
         when T_IF     => Result := Parse_If_Clause (B, T, C);
         when T_CASE   => Result := Parse_Case_Clause (B, T, C);
         when T_FOR    => Result := Parse_For_Clause (B, T, C);
         when T_LPAR   => Result := Parse_Subshell (B, T, C);
         when T_LBRACE => Result := Parse_Brace_Group (B, T, C);
         when others   => null;
      end case;

      --  Parse if necessary redirections on compound commands
      if Is_Redirection_Op (Lookahead_Command (B), True) then
         Parse_Redirect_List (B, T, C, Result);
      end if;

      return Result;
   end Parse_Compound_Command;

   -------------------------
   -- Parse_Compound_List --
   -------------------------

   function Parse_Compound_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      N : Node_Id;
   begin
      Parse_Linebreak (B, T, C);
      N := Parse_Term (B, T, C);

      --  Following code disabled as Parse_Term has already taken care of seps
      --  case Lookahead (B) is
      --     when T_SEMI | T_AND | T_NEWLINE =>
      --       Parse_Separator (B, C);
      --     when others => null;
      --  end case;
      return N;
   end Parse_Compound_List;

   --------------------
   -- Parse_Do_Group --
   --------------------

   function Parse_Do_Group
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      pragma Unreferenced (C);
      Result : Node_Id;
   begin
      Expect_Token (B, T_DO, "expect token 'do'");
      Result := Parse_Compound_List (B, T, DO_GROUP_CONTEXT);
      Expect_Token (B, T_DONE);
      return Result;
   end Parse_Do_Group;

   ---------------------
   -- Parse_Else_Part --
   ---------------------

   function Parse_Else_Part
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Result : Node_Id;
   begin
      --  Parse_Else_Part is only called by Parse_If_Clause when next token is
      --  'else' or 'elif'
      pragma Assert (Lookahead_Command (B) = T_ELSE or else
                     Lookahead_Command (B) = T_ELIF);

      if Lookahead_Command (B) = T_ELSE then
         Skip_Token (B);
         Result := Parse_Compound_List (B, T, IF_ELSE_CONTEXT);
      else
         Result := Parse_If_Clause (B, T, C, True);
      end if;
      return Result;
   end Parse_Else_Part;

   ----------------
   -- Parse_File --
   ----------------

   function Parse_File (Filename : String) return Shell_Tree is
      B : Token_Buffer;
      N : Node_Id := 0;
      T : Shell_Tree;
   begin
      B := New_Buffer_From_File (Filename);
      T := New_Tree (B.B);
      N := Parse (B, T);
      --  XXX deallocate buffer and tree
      Set_Tree_Toplevel (T, N);
      return T;
   end Parse_File;

   ----------------------
   -- Parse_For_Clause --
   ----------------------

   function Parse_For_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Current            : constant Token := Read_Command_Token (B);
      Variable_Name      : Token;
      Value_List         : Token_List := Null_List;
      Loop_Code          : Node_Id := Null_Node;
      Default_Value_List : Boolean := False;
   begin
      pragma Assert (Get_Token_Type (Current) = T_FOR);

      --  Get the loop variable name
      Variable_Name := Read_Word_Token (B);

      Parse_Linebreak (B, T, C);

      --  look for 'in' keyword
      if Lookahead_Command (B) = T_IN then
         Skip_Token (B);

         --   get the list of value for the loop variable
         case Lookahead (B) is
            when T_SEMI | T_NEWLINE => null;
            when others => Value_List := Parse_Word_List (B, T, C);
         end case;
         Parse_Sequential_Sep (B, T, C);
      else
         Default_Value_List := True;
         case Lookahead (B) is
            when T_SEMI | T_NEWLINE => Parse_Sequential_Sep (B, T, C);
            when others => null;
         end case;

      end if;

      --  get the loop commands
      Loop_Code := Parse_Do_Group (B, T, C);

      return Add_For_Node
        (T,
         Variable_Name,
         Value_List,
         Loop_Code,
         Default_Value_List);
   end Parse_For_Clause;

   ---------------------
   -- Parse_If_Clause --
   ---------------------

   function Parse_If_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Elif_Mode : Boolean := False)
      return Node_Id
   is
      pragma Unreferenced (C);
      Cond, True_Code, False_Code : Node_Id := Null_Node;
   begin

      pragma Assert
        ((Lookahead_Command (B) = T_IF and not Elif_Mode) or else
           (Lookahead_Command (B) = T_ELIF and Elif_Mode));

      Skip_Token (B);

      --  Parse condition
      Cond := Parse_Compound_List (B, T, IF_COND_CONTEXT);

      --  THEN ... part parsing

      pragma Assert (Lookahead_Command (B) = T_THEN);
      Skip_Token (B);

      True_Code := Parse_Compound_List (B, T, IF_THEN_CONTEXT);

      case Lookahead_Command (B) is
         when T_ELSE | T_ELIF =>
            False_Code := Parse_Else_Part (B, T, IF_ELSE_CONTEXT);
            if not Elif_Mode then
               Expect_Token (B, T_FI);
            end if;
         when T_FI =>
            if not Elif_Mode then
               Skip_Token (B);
            end if;
         when others =>
            Syntax_Error (Read_Token (B), "expect 'fi'");
      end case;

      return Add_If_Node (T, Cond, True_Code, False_Code);
   end Parse_If_Clause;

   -------------------
   -- Parse_IO_File --
   -------------------

   procedure Parse_IO_File
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      N : Node_Id;
      IO_Number : Integer)
   is
      pragma Unreferenced (C);
      Target_FD : Integer := IO_Number;
      IO_Mode   : constant Token_Type := Read_Token (B);
      Filename  : Token;
   begin
      pragma Assert (IO_Mode in Redirection_Ops'Range);

      --  First if no file descriptor is specified as operator prefix, we
      --  calculate it which is the implicit one (stdin or stdout)
      if Target_FD = -1 then
         if Is_Input_Redirection_Op (IO_Mode) then
            Target_FD := 0;
         else
            Target_FD := 1;
         end if;
      end if;

      Filename := Read_Word_Token (B);
      case IO_Mode is
         when T_LESS =>
            Set_Node_Redirection
              (T, N, (OPEN_READ, Target_FD, Filename));
         when T_GREAT =>
            Set_Node_Redirection
              (T, N, (OPEN_WRITE, Target_FD, Filename));
         when T_DGREAT =>
            Set_Node_Redirection
              (T, N, (OPEN_APPEND, Target_FD, Filename));
         when T_GREATAND =>
            Set_Node_Redirection
              (T, N, (DUPLICATE, Target_FD, Filename));
         when T_DLESS | T_DLESSDASH =>
            --  This implem is not right need to fix at some point XXXX

            --  Add_Pending_IO_Here (N, Filename, Target_FD);
            --  Parse_Redirect_List (B, C, N);
            Pending_IO_Heres_Last := Pending_IO_Heres_Last + 1;
            Pending_IO_Heres (Pending_IO_Heres_Last) :=
              (N, Filename, Target_FD);
            --  Set_Node_Redirection
            --  (N, Target_FD, Read_IOHere (B, Filename), 0, IOHERE);
            --  After IOHere reading invalidate any cache
            --- B.Valid_Cache := False;

         when others =>
            null;
      end case;
   end Parse_IO_File;

   -----------------------
   -- Parse_IO_Redirect --
   -----------------------

   procedure Parse_IO_Redirect
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      N : Node_Id)
   is
      IO_Number : Integer := -1;
   begin
      if Lookahead (B) = T_IO_NUMBER then
         IO_Number := Integer'Value (Get_Token_String (Read_Token (B)));
      end if;

      Parse_IO_File (B, T, C, N, IO_Number);
   end Parse_IO_Redirect;

   ---------------------
   -- Parse_Linebreak --
   ---------------------

   procedure Parse_Linebreak
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
   is
      pragma Unreferenced (C);
   begin
      if Lookahead (B) = T_NEWLINE then
         for Index in 1 .. Pending_IO_Heres_Last loop
            declare
               Eval : Boolean;
               IOHere_Token : constant Token :=
                 Read_IOHere (B, Pending_IO_Heres (Index).Marker, Eval);
            begin
               Set_Node_Redirection
                 (T,
                  Pending_IO_Heres (Index).N,
                  (IOHERE,
                   Pending_IO_Heres (Index).Target_Fd,
                   IOHere_Token,
                   Eval));
               B.Valid_Cache := False;
            end;
         end loop;
         Pending_IO_Heres_Last := 0;
      end if;

      while Lookahead (B) =  T_NEWLINE loop
         Skip_Token (B);
      end loop;
   end Parse_Linebreak;

   ----------------
   -- Parse_List --
   ----------------

   function Parse_List
     (B           : in out Token_Buffer;
      Tree        : in out Shell_Tree;
      Context     : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is
      --  use Node_Id_Tables;

      --  Childs      : Instance;
      --  Child_Index : Integer := 1;
      Current     : Node_Id := Null_Node;
      Prev        : Node_Id := Null_Node;
      Result      : Node_Id;
   begin
      --  Init (Childs);

      Current := Parse_And_Or (B, Tree, Context, Until_Token => Until_Token);
      Prev := Current;
      Result := Current;

      --  Set_Item (Childs, Child_Index, Current);
      --  Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_SEMI | T_AND | T_NEWLINE =>
               Parse_Separator (B, Tree, Context);

               exit when Lookahead (B) = T_EOF;

               Current := Parse_And_Or
                 (B, Tree, Context, Until_Token => Until_Token);
               if Current = 0 then
                  exit;
               end if;

               Set_Node_Continuation (Tree, Prev, Current, Current);
               Prev := Current;
               --  Set_Item (Childs, Child_Index, Current);
               --  Child_Index := Child_Index + 1;
               pragma Assert (Current /= Null_Node);
            when T_EOF =>
               exit;
            when others =>
               if Until_Token /= T_NULL and then
                 Until_Token = Lookahead (B)
               then
                  exit;
               end if;
               declare
                  T : constant Token := Read_Token (B);
                  pragma Unreferenced (T);
               begin
                  raise Shell_Syntax_Error;
               end;
         end case;
      end loop;

      --  if Last (Childs) = 1 then
      --           Result := Childs.Table (1);
      --        else
      --           Result := Add_List_Node
      --             (Tree, Node_Id_Array (Childs.Table (1 .. Last (Childs))));
      --        end if;
      --
      --        Free (Childs);
      return Result;

   end Parse_List;

   -------------------
   -- Parse_Pattern --
   -------------------

   function Parse_Pattern (B : in out Token_Buffer; T : in out Shell_Tree)
     return Token_List
   is
      S      : Token := Read_Word_Token (B);
      Result : Token_List := Null_List;
   begin
      loop
         Append (T, Result, S);
         if Lookahead (B) = T_PIPE then
            Skip_Token (B);
            S := Read_Word_Token (B);
         else
            return Result;
         end if;
      end loop;
   end Parse_Pattern;

   -------------------------
   -- Parse_Pipe_Sequence --
   -------------------------

   function Parse_Pipe_Sequence
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Pipe_Negation : Boolean;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is
      --  Left, Right : Node_Id := Null_Node;
      use Node_Id_Tables;

      Childs      : Instance;
      Child_Index : Integer := 1;
      Current     : Node_Id := Null_Node;
      Result      : Node_Id;
   begin
      Init (Childs);

      Current := Parse_Command (B, T, C, Until_Token => Until_Token);
      Set_Item (Childs, Child_Index, Current);
      Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_PIPE =>
               Skip_Token (B);
               Parse_Linebreak (B, T, C);

               Current := Parse_Command (B, T, C, Until_Token => Until_Token);

               if Current = 0 then
                  exit;
               end if;
               Set_Item (Childs, Child_Index, Current);
               Child_Index := Child_Index + 1;
               pragma Assert (Current /= Null_Node);
            when others =>
               exit;
         end case;
      end loop;

      if Last (Childs) = 1 and then not Pipe_Negation then
         Result := Childs.Table (1);
      else
         Result := Add_Pipe_Node
           (T, Node_Id_Array
              (Childs.Table (1 .. Last (Childs))), Pipe_Negation);
      end if;

      Free (Childs);
      return Result;

--        --  Left := Parse_Command (B, T, C);
--        case Lookahead (B) is
--           when T_PIPE =>
--              Skip_Token (B);
--              Parse_Linebreak (B, T, C);
--              Right := Parse_Pipe_Sequence (B, T, C, False);
--              return Add_Pipe_Node (T, Left, Right, Pipe_Negation);
--           when others =>
--              if Pipe_Negation then
--                 return Add_Pipe_Node (T, Left, 0, Pipe_Negation);
--              else
--                 return Left;
--              end if;
--        end case;
   end Parse_Pipe_Sequence;

   --------------------
   -- Parse_Pipeline --
   --------------------

   function Parse_Pipeline
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      Until_Token : Token_Type := T_NULL)
      return Node_Id
   is
      Pipe_Negation : Boolean := False;
   begin
      case Lookahead_Command (B) is
         when T_BANG =>
            Skip_Token (B);
            Pipe_Negation := True;
         when others => null;
      end case;

      return  Parse_Pipe_Sequence (B, T, C, Pipe_Negation,
                                   Until_Token => Until_Token);
   end Parse_Pipeline;

   -------------------------
   -- Parse_Redirect_List --
   -------------------------

   procedure Parse_Redirect_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context;
      N : Node_Id)
   is
      Current : Token_Type;
   begin
      loop
         Current := Lookahead (B);
         if Is_Redirection_Op (Current, True) then
            Parse_IO_Redirect (B, T, C, N);
         else
            exit;
         end if;
      end loop;
   end Parse_Redirect_List;

   ---------------------
   -- Parse_Separator --
   ---------------------

   procedure Parse_Separator
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context) is
      Current : constant Token_Type := Lookahead (B);
   begin
      pragma Assert (Current = T_SEMI
                     or else Current = T_AND
                     or else Current = T_NEWLINE);
      if Current = T_SEMI or Current = T_AND then
         Skip_Token (B);
      end if;
      Parse_Linebreak (B, T, C);
   end Parse_Separator;

   --------------------------
   -- Parse_Sequentiel_Sep --
   --------------------------

   procedure Parse_Sequential_Sep
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
   is
      Cur : constant Token_Type := Lookahead (B);
   begin
      pragma Assert (Cur = T_SEMI or else Cur = T_NEWLINE);
      if Cur = T_SEMI then
         Skip_Token (B);
      end if;
      Parse_Linebreak (B, T, C);
   end Parse_Sequential_Sep;

   --------------------------
   -- Parse_Simple_Command --
   --------------------------

   function Parse_Simple_Command
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
     return Node_Id
   is
      --  Assign_List : Annotated_String_List_Access := null;
      Cmd        : Token;
      Cmd_Id     : constant Node_Id := Add_Null_Node (T);
      Cur        : Token;
      CurT       : Token_Type;
      Is_Pos_Set : Boolean := False;
   begin
      pragma Debug (Log ("parse_simple_command", ""));
      pragma Assert (Lookahead_Command (B) = T_ASSIGNEMENT or else
                     Lookahead_Command (B) = T_WORD or else
                     Is_Redirection_Op (Lookahead_Command (B), True));

      --  cmd_prefix

      CurT := Lookahead_Command (B);
      pragma Debug (Log ("parse_simple_command", CurT'Img));
      while CurT = T_ASSIGNEMENT or else Is_Redirection_Op (CurT, True) loop
         if CurT = T_ASSIGNEMENT then
            Cur := Read_Command_Token (B);
            if not Is_Pos_Set then
               Set_Node_Pos (T, Cmd_Id, Get_Token_Pos (Cur));
               Is_Pos_Set := True;
            end if;
            pragma Debug (Log ("append_assignment", Get_Token_String (Cur)));
            Append_Assignement (T, Cmd_Id, Cur);
         else
            Parse_IO_Redirect (B, T, C, Cmd_Id);
         end if;
         CurT := Lookahead_Command (B);
      end loop;

      if Lookahead (B) = T_WORD then
         Cur := Read_Token (B);
         pragma Debug (Log ("parse_cmd", Get_Token_String (Cur)));
         if not Is_Pos_Set then
            Set_Node_Pos (T, Cmd_Id, Get_Token_Pos (Cur));
            Is_Pos_Set := True;
         end if;

         Cmd := Cur;

         --  This is a kind of hack for functions
         if Lookahead (B) = T_LPAR then
            --  this is a function declaration
            Skip_Token (B);
            Expect_Token (B, T_RPAR);
            Parse_Linebreak (B, T, C);
            declare
               Function_Tree : Shell_Tree := New_Tree
                 (B.B, Protect => True, Allow_Return => True);
               N : constant Node_Id := Parse_Compound_Command
                 (B, Function_Tree, C);
            begin
               Set_Tree_Toplevel (Function_Tree, N);
               Set_Function_Node (T, Cmd_Id, Cmd, Function_Tree);

               --  tree associated with a function cannot be deallocated along
               --  with the buffer that is also the buffer of the parent tree
               Protect_Tree_Buffer (T);
            end;
         else

            Set_Cmd_Node (T, Cmd_Id, Cmd);

            --  cmd_suffix
            CurT := Lookahead (B);

            while CurT = T_WORD or else Is_Redirection_Op (CurT, True) loop

               if CurT = T_WORD then
                  pragma Debug (Log ("append_arg", ""));
                  Append_Arg (T, Cmd_Id, Read_Word_Token (B));
               else
                  Parse_IO_Redirect (B, T, C, Cmd_Id);
               end if;
               CurT := Lookahead (B);
            end loop;
         end if;
      end if;

      return Cmd_Id;
   end Parse_Simple_Command;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (S : String)
      return Shell_Tree
   is
      B : Token_Buffer;
      T : Shell_Tree;
      N : Node_Id := 0;
   begin
      B := New_Buffer (S);
      T := New_Tree (B.B);
      N := Parse (B, T);
      Set_Tree_Toplevel (T, N);
      return T;
      --  XXX missing deallocation of buffer.
   end Parse_String;

   --------------------
   -- Parse_Subshell --
   --------------------

   function Parse_Subshell
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      pragma Unreferenced (C);
      Subshell_Code : Node_Id := Null_Node;
   begin
      pragma Assert (Lookahead_Command (B) = T_LPAR);
      Skip_Token (B);
      Subshell_Code := Parse_Compound_List (B, T, SUBSHELL_CONTEXT); -- XXXXX
      Expect_Token (B, T_RPAR);
      return Add_Subshell_Node (T, Subshell_Code);
   end Parse_Subshell;

   ----------------
   -- Parse_Term --
   ----------------

   function Parse_Term
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      --  use Node_Id_Tables;
      --  Childs : Instance;
      --  Child_Index : Integer := 1;
      Current : Node_Id := Null_Node;
      Prev    : Node_Id := Null_Node;
      Result  : Node_Id;
   begin
      --  Init (Childs);

      Current := Parse_And_Or (B, T, C);
      Result := Current;
      Prev := Current;

      --  Set_Item (Childs, Child_Index, Current);
      --  Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_SEMI | T_AND | T_NEWLINE =>
               Parse_Separator (B, T, C);
               Current := Parse_And_Or (B, T, C);
               if Current = 0 then
                  exit;
               end if;
               Set_Node_Continuation (T, Prev, Current, Current);
               Prev := Current;
               --  Set_Item (Childs, Child_Index, Current);
               --  Child_Index := Child_Index + 1;
            when others =>
               exit;
         end case;
      end loop;

      --        if Last (Childs) = 1 then
      --           Result := Childs.Table (1);
      --        else
      --           Result := Add_List_Node
      --             (T, Node_Id_Array (Childs.Table (1 .. Last (Childs))));
      --        end if;
      --
      --        Free (Childs);
      return Result;

   end Parse_Term;

   ------------------------
   -- Parse_Until_Clause --
   ------------------------

   function Parse_Until_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Cond, Loop_Code : Node_Id := Null_Node;
   begin
      Skip_Token (B);
      Cond := Parse_Compound_List (B, T, LOOP_COND_CONTEXT);
      Loop_Code := Parse_Do_Group (B, T, C);
      return Add_Until_Node (T, Cond, Loop_Code);

   end Parse_Until_Clause;

   ------------------------
   -- Parse_While_Clause --
   ------------------------

   function Parse_While_Clause
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Node_Id
   is
      Cond, Loop_Code : Node_Id := Null_Node;
   begin
      Skip_Token (B);
      Cond := Parse_Compound_List (B, T, LOOP_COND_CONTEXT);
      Loop_Code := Parse_Do_Group (B, T, C);

      return Add_While_Node (T, Cond, Loop_Code);
   end Parse_While_Clause;

   ---------------------
   -- Parse_Word_List --
   ---------------------

   function Parse_Word_List
     (B : in out Token_Buffer;
      T : in out Shell_Tree;
      C : Parser_Context)
      return Token_List
   is
      Result : Token_List := Null_List;
      Value  : Token := Read_Word_Token (B);
   begin
      loop
         case Lookahead (B) is
            when T_WORD =>
               if C = BRACEGROUP_CONTEXT and then
                 Lookahead_Command (B) = T_RBRACE
               then
                  Append (T, Result, Value);
                  return Result;
               end if;

               Append (T, Result, Value);
            when others =>
               Append (T, Result, Value);
               return Result;
         end case;
         Value := Read_Word_Token (B);
      end loop;
   end Parse_Word_List;

end Sh.Parser;
