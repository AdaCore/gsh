with Ada.Text_IO;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with Posix_Shell.Variables.Output;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Annotated_String_Lists; use Annotated_String_Lists;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package body Posix_Shell.Parser is

   type Context is
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
      Marker    : Annotated_String := Null_Annotated_String;
      Target_Fd : Integer;
   end record;
   pragma Warnings (On);

   type IO_Here_Context_Array is array (1 .. 8) of IO_Here_Context;
   Pending_IO_Heres      : IO_Here_Context_Array;
   Pending_IO_Heres_Last : Natural := 0;

   function Parse (B : Buffer_Access; T : Shell_Tree_Access) return Node_Id;
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  list : list separator_op and_or
   --       |                   and_or
   --       ;

   function Parse_And_Or
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  and_or :                         pipeline
   --         | and_or AND_IF linebreak pipeline
   --         | and_or OR_IF  linebreak pipeline
   --         ;

   function Parse_Pipeline
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  pipeline :      pipe_sequence
   --           | Bang pipe_sequence
   --           ;

   function Parse_Pipe_Sequence
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      Pipe_Negation : Boolean) return Node_Id;
   --  pipe_sequence :                             command
   --                | pipe_sequence '|' linebreak command
   --                ;

   function Parse_Command
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  command          : simple_command
   --                   | compound_command
   --                   | compound_command redirect_list
   --                   | function_definition
   --                   ;

   function Parse_Compound_Command
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  subshell         : '(' compound_list ')'
   --                   ;

   function Parse_Compound_List
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  compound_list    :              term
   --                   | newline_list term
   --                   |              term separator
   --                   | newline_list term separator
   --                   ;

   function Parse_Term
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  term             : term separator and_or
   --                   |                and_or
   --                   ;

   function Parse_For_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  for_clause      : For name linebreak                            do_group
   --                  | For name linebreak in          sequential_sep do_group
   --                  | For name linebreak in wordlist sequential_sep do_group
   --                  ;

   function Parse_Word_List
     (B : Buffer_Access;
      C : Context)
      return Annotated_String_List;
   --  wordlist         : wordlist WORD
   --                   |          WORD
   --                   ;

   function Parse_Case_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context) return Node_Id;
   --  case_clause      : Case WORD linebreak in linebreak case_list    Esac
   --                   | Case WORD linebreak in linebreak case_list_ns Esac
   --                   | Case WORD linebreak in linebreak              Esac
   --                   ;

   function Parse_Case_List
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  case_list_ns     : case_list case_item_ns
   --                   |           case_item_ns
   --                   ;
   --  case_list        : case_list case_item
   --                   |           case_item
   --                   ;

   function Parse_Pattern (B : Buffer_Access)
     return Annotated_String_List;
   --  pattern          :             WORD         /* Apply rule 4 */
   --                   | pattern '|' WORD         /* Do not apply rule 4 */
   --                   ;

   function Parse_If_Clause
     (B         : Buffer_Access;
      T : Shell_Tree_Access;
      C         : Context;
      Elif_Mode : Boolean := False)
      return Node_Id;
   --  if_clause        : If compound_list Then compound_list else_part Fi
   --                   | If compound_list Then compound_list           Fi
   --                   ;

   function Parse_Else_Part
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  else_part        : Elif compound_list Then else_part
   --                   | Else compound_list
   --                   ;

   function Parse_While_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context) return Node_Id;
   --  while_clause     : While compound_list do_group
   --                   ;

   function Parse_Until_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  until_clause  : Until compound_list do_group
   --                ;

   function Parse_Brace_Group
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  brace_group      : Lbrace compound_list Rbrace
   --                   ;

   function Parse_Do_Group
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id;
   --  do_group         : Do compound_list Done           /* Apply rule 6 */
   --                   ;

   function Parse_Simple_Command
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context; N : Node_Id);
   --  redirect_list    :               io_redirect
   --                   | redirect_list io_redirect
   --                   ;

   procedure Parse_IO_Redirect
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      N : Node_Id);
   --  io_redirect      :           io_file
   --                   | IO_NUMBER io_file
   --                   |           io_here
   --                   | IO_NUMBER io_here
   --                   ;

   procedure Parse_Linebreak
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context);
   pragma Inline (Parse_Linebreak);
   --  newline_list     :              NEWLINE
   --                   | newline_list NEWLINE
   --                   ;
   --  linebreak        : newline_list
   --                   | /* empty */
   --                   ;

   procedure Parse_Separator
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context);
   pragma Inline (Parse_Separator);
   --  separator_op     : '&'
   --                   | ';'
   --                   ;
   --  separator        : separator_op linebreak
   --                   | newline_list
   --                   ;

   procedure Parse_Sequential_Sep
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context);

   pragma Inline (Parse_Sequential_Sep);
   --  sequential_sep   : ';' linebreak
   --                   | newline_list
   --                   ;

   procedure Parse_IO_File
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      N : Node_Id;
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

   function Parse (B : Buffer_Access; T : Shell_Tree_Access) return Node_Id is
   begin
      while Lookahead (B) = T_NEWLINE loop
         Skip_Token (B);
      end loop;
      return Parse_List (B, T, NULL_CONTEXT);
   end Parse;

   ------------------
   -- Parse_And_Or --
   ------------------

   function Parse_And_Or
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is

      use And_Or_Node_Id_Tables;

      Childs : Instance;
      Child_Index : Integer := 1;
      Current : Node_Id := Null_Node;
      Result : Node_Id;
      Current_Kind : Token_Type;
   begin
      Init (Childs);

      Current := Parse_Pipeline (B, T, C);
      Set_Item (Childs, Child_Index, (Current, AND_LIST));
      Child_Index := Child_Index + 1;

      loop
         Current_Kind := Lookahead (B);
         case Current_Kind is
            when T_OR_IF | T_AND_IF =>
               Skip_Token (B);
               Parse_Linebreak (B, T, C);

               Current := Parse_Pipeline (B, T, C);
               if Current = 0 then
                  exit;
               end if;
               if Current_Kind = T_OR_IF then
                  Set_Item (Childs, Child_Index, (Current, OR_LIST));
               else
                  Set_Item (Childs, Child_Index, (Current, AND_LIST));
               end if;
               Child_Index := Child_Index + 1;
               pragma Assert (Current /= Null_Node);
            when others =>
               exit;
         end case;
      end loop;

      if Last (Childs) = 1 then
         Result := Childs.Table (1).N;
      else
         Result := Add_And_Or_List_Node
           (T, And_Or_Node_Id_Array (Childs.Table (1 .. Last (Childs))));
      end if;

      Free (Childs);
      return Result;
   end Parse_And_Or;

   -----------------------
   -- Parse_Brace_Group --
   -----------------------

   function Parse_Brace_Group
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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

   -----------------------
   -- Parse_Case_Clause --
   -----------------------

   function Parse_Case_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      Case_Value : Annotated_String;
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      Pattern        : Annotated_String_List;
      Case_Code      : Node_Id;
      Next_Case_Code : Node_Id;
   begin
      if Lookahead_Command (B) = T_ESAC or else
        Lookahead_Command (B) = T_EOF
      then
         return Null_Node;
      end if;

      Pattern  := Parse_Pattern (B);

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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      Result : Node_Id := 0;

      procedure Context_Syntax_Error (Expected : Context);

      --------------------------
      -- Context_Syntax_Error --
      --------------------------

      procedure Context_Syntax_Error (Expected : Context) is
      begin
         if C /= Expected then
            Syntax_Error (Read_Token (B), "non expected token");
         end if;
      end Context_Syntax_Error;
      pragma Inline (Context_Syntax_Error);

   begin
      case Lookahead_Command (B) is
         when T_UNTIL | T_WHILE | T_IF | T_CASE | T_FOR | T_LPAR | T_LBRACE =>
            Result := Parse_Compound_Command (B, T, C);
         when T_WORD | T_ASSIGNEMENT | T_GREAT | T_GREATAND | T_DGREAT |
              T_CLOBBER | T_LESS | T_LESSAND | T_LESSGREAT | T_DLESS |
              T_IO_NUMBER =>
            Result := Parse_Simple_Command (B, T, C);
         when T_THEN   => Context_Syntax_Error (IF_COND_CONTEXT);
         when T_DONE   => Context_Syntax_Error (DO_GROUP_CONTEXT);
         when T_DO     => Context_Syntax_Error (LOOP_COND_CONTEXT);
         when T_RPAR   => Context_Syntax_Error (SUBSHELL_CONTEXT);
         when T_DSEMI  => Context_Syntax_Error (CASE_ITEM_CONTEXT);
         when T_ESAC   => Context_Syntax_Error (CASE_ITEM_CONTEXT);
         when T_RBRACE => Context_Syntax_Error (BRACEGROUP_CONTEXT);
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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

   function Parse_File (Filename : String) return Shell_Tree_Access is
      B : Buffer_Access;
      N : Node_Id := 0;
      T : constant Shell_Tree_Access := New_Tree;
   begin
      B := new Token_Buffer;
      B.all := New_Buffer_From_File (Filename);
      N := Parse (B, T);
      --  XXX deallocate buffer and tree
      Set_Tree_Toplevel (T, N);
      return T;
   end Parse_File;

   ----------------------
   -- Parse_For_Clause --
   ----------------------

   function Parse_For_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      Current       : constant Token := Read_Command_Token (B);
      Variable_Name : Annotated_String;
      Value_List    : Annotated_String_List := Null_Annotated_String_List;
      Loop_Code     : Node_Id            := Null_Node;
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
            when others => Value_List := Parse_Word_List (B, C);
         end case;
         Parse_Sequential_Sep (B, T, C);
      else
         declare
            Tmp : Annotated_String;
         begin
            Append (Tmp, '"', DOUBLE_QUOTE_BEGIN);
            Append (Tmp, '$', PARAM_EVAL_BEGIN);
            Append (Tmp, '@', PARAM_EVAL_END);
            Append (Tmp, '"', DOUBLE_QUOTE_END);

            Append (Value_List, Tmp);
         end;
      end if;

      --  get the loop commands
      Loop_Code := Parse_Do_Group (B, T, C);

      return Add_For_Node (T, Variable_Name, Value_List, Loop_Code);
   end Parse_For_Clause;

   ---------------------
   -- Parse_If_Clause --
   ---------------------

   function Parse_If_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      N : Node_Id;
      IO_Number : Integer)
   is
      pragma Unreferenced (C);
      use Posix_Shell.Variables.Output;
      Target_FD : Integer := IO_Number;
      IO_Mode   : constant Token_Type := Read_Token (B);
      Filename  : Annotated_String;
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
              (T.all, N, Target_FD, Filename, 0, OPEN_READ);
         when T_GREAT =>
            Set_Node_Redirection
              (T.all, N, Target_FD, Filename, 0, OPEN_WRITE);
         when T_DGREAT =>
            Set_Node_Redirection
              (T.all, N, Target_FD, Filename, 0, OPEN_APPEND);
         when T_GREATAND =>
            declare
               Source_FD : Integer := 0;
               Is_Valid  : Boolean := False;
            begin
               To_Integer (Str (Filename), Source_FD, Is_Valid);
               if Is_Valid then
                  Set_Node_Redirection
                    (T.all, N, Target_FD, Filename, Source_FD, DUPLICATE);
               else
                  Set_Node_Redirection
                    (T.all, N, Target_FD, Filename, 0, OPEN_WRITE);
               end if;
            end;
         when T_DLESS =>
            --  This implem is not right need to fix at some point XXXX

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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      N : Node_Id)
   is
      IO_Number : Integer := -1;
   begin
      if Lookahead (B) = T_IO_NUMBER then
         IO_Number := Integer'Value (Str (Read_Token (B)));
      end if;

      Parse_IO_File (B, T, C, N, IO_Number);
   end Parse_IO_Redirect;

   ---------------------
   -- Parse_Linebreak --
   ---------------------

   procedure Parse_Linebreak
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
   is
      pragma Unreferenced (C);
   begin
      if Lookahead (B) = T_NEWLINE then
         for Index in 1 .. Pending_IO_Heres_Last loop
            Set_Node_Redirection
              (T.all,
               Pending_IO_Heres (Index).N,
               Pending_IO_Heres (Index).Target_Fd,
               Read_IOHere (B, Pending_IO_Heres (Index).Marker),
               0,
               Posix_Shell.Variables.Output.IOHERE);
            B.Valid_Cache := False;
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      use Node_Id_Tables;

      Childs      : Instance;
      Child_Index : Integer := 1;
      Current     : Node_Id := Null_Node;
      Result      : Node_Id;
   begin
      Init (Childs);

      Current := Parse_And_Or (B, T, C);
      Set_Item (Childs, Child_Index, Current);
      Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_SEMI | T_AND | T_NEWLINE =>
               Parse_Separator (B, T, C);
               exit when Lookahead (B) = T_EOF;
               Current := Parse_And_Or (B, T, C);
               if Current = 0 then
                  exit;
               end if;
               Set_Item (Childs, Child_Index, Current);
               Child_Index := Child_Index + 1;
               pragma Assert (Current /= Null_Node);
            when T_EOF =>
               exit;
            when others =>
               declare
                  T : constant Token := Read_Token (B);
               begin
                  Ada.Text_IO.Put_Line (Token_Pos_Img (T));
               --  Syntax_Error (Read_Token (B), "non expected token");
                  raise Shell_Syntax_Error;
               end;
         end case;
      end loop;

      if Last (Childs) = 1 then
         Result := Childs.Table (1);
      else
         Result := Add_List_Node
           (T, Node_Id_Array (Childs.Table (1 .. Last (Childs))));
      end if;

      Free (Childs);
      return Result;

   end Parse_List;

   -------------------
   -- Parse_Pattern --
   -------------------

   function Parse_Pattern (B : Buffer_Access)
     return Annotated_String_List
   is
      S  : Annotated_String := Read_Word_Token (B);
      Result : Annotated_String_List := Null_Annotated_String_List;
   begin
      loop
         Append (Result, S);
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context;
      Pipe_Negation : Boolean)
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

      Current := Parse_Command (B, T, C);
      Set_Item (Childs, Child_Index, Current);
      Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_PIPE =>
               Skip_Token (B);
               Parse_Linebreak (B, T, C);

               Current := Parse_Command (B, T, C);

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

      if Last (Childs) = 1 then
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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

      return  Parse_Pipe_Sequence (B, T, C, Pipe_Negation);
   end Parse_Pipeline;

   -------------------------
   -- Parse_Redirect_List --
   -------------------------

   procedure Parse_Redirect_List
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context; N : Node_Id)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context) is
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
     return Node_Id
   is
      --  Assign_List : Annotated_String_List_Access := null;
      Cmd         : Annotated_String;
      Cmd_Id      : constant Node_Id := Add_Null_Node (T);
      Cur           : Token;
      CurT          : Token_Type;
      Is_Pos_Set  : Boolean := False;
   begin
      pragma Assert (Lookahead_Command (B) = T_ASSIGNEMENT or else
                     Lookahead_Command (B) = T_WORD or else
                     Is_Redirection_Op (Lookahead_Command (B), True));

      --  cmd_prefix
      CurT := Lookahead_Command (B);
      while CurT = T_ASSIGNEMENT or else Is_Redirection_Op (CurT, True) loop
         if CurT = T_ASSIGNEMENT then
            Cur := Read_Command_Token (B);
            if not Is_Pos_Set then
               Set_Node_Pos (T.all, Cmd_Id, Get_Token_Pos (Cur));
               Is_Pos_Set := True;
            end if;
            Append_Assignement (T, Cmd_Id, Get_Token_String (Cur));
         else
            Parse_IO_Redirect (B, T, C, Cmd_Id);
         end if;
         CurT := Lookahead_Command (B);
      end loop;

      if Lookahead (B) = T_WORD then
         Cur := Read_Token (B);
         if not Is_Pos_Set then
            Set_Node_Pos (T.all, Cmd_Id, Get_Token_Pos (Cur));
            Is_Pos_Set := True;
         end if;

         Cmd := Get_Token_String (Cur);

         --  This is a kind of hack for functions
         if Lookahead (B) = T_LPAR then
            --  this is a function declaration
            Skip_Token (B);
            Expect_Token (B, T_RPAR);
            Parse_Linebreak (B, T, C);
            declare
               Function_Tree : constant Shell_Tree_Access := New_Tree;
               N : constant Node_Id := Parse_Compound_Command
                 (B, Function_Tree, C);
            begin
               Set_Tree_Toplevel (Function_Tree, N);
               Set_Function_Node (T, Cmd_Id, Cmd, Function_Tree);
            end;
         else

            Set_Cmd_Node (T, Cmd_Id, Cmd);

            --  cmd_suffix
            CurT := Lookahead (B);
            while CurT = T_WORD or else Is_Redirection_Op (CurT, True) loop

               if CurT = T_WORD then
                  Append_Arg (T.all, Cmd_Id, Read_Word_Token (B));
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

   function Parse_String (S : String) return Shell_Tree_Access is
      B : Buffer_Access;
      T : constant Shell_Tree_Access := New_Tree;
      N : Node_Id := 0;
   begin
      B := new Token_Buffer;
      B.all := New_Buffer (S);
      N := Parse (B, T);
      Set_Tree_Toplevel (T, N);
      Deallocate (B);
      return T;
      --  XXX missing deallocation of buffer.
   end Parse_String;

   --------------------
   -- Parse_Subshell --
   --------------------

   function Parse_Subshell
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
      return Node_Id
   is
      use Node_Id_Tables;

      Childs : Instance;
      Child_Index : Integer := 1;
      Current : Node_Id := Null_Node;
      Result : Node_Id;
   begin
      Init (Childs);

      Current := Parse_And_Or (B, T, C);
      Set_Item (Childs, Child_Index, Current);
      Child_Index := Child_Index + 1;

      loop
         case Lookahead (B) is
            when T_SEMI | T_AND | T_NEWLINE =>
               Parse_Separator (B, T, C);
               Current := Parse_Term (B, T, C);
               if Current = 0 then
                  exit;
               end if;
               Set_Item (Childs, Child_Index, Current);
               Child_Index := Child_Index + 1;
            when others =>
               exit;
         end case;
      end loop;

      if Last (Childs) = 1 then
         Result := Childs.Table (1);
      else
         Result := Add_List_Node
           (T, Node_Id_Array (Childs.Table (1 .. Last (Childs))));
      end if;

      Free (Childs);
      return Result;

   end Parse_Term;

   ------------------------
   -- Parse_Until_Clause --
   ------------------------

   function Parse_Until_Clause
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      T : Shell_Tree_Access;
      C : Context)
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
     (B : Buffer_Access;
      C : Context)
      return Annotated_String_List
   is
      Result : Annotated_String_List := Null_Annotated_String_List;
      Value : Annotated_String := Read_Word_Token (B);
   begin
      loop
         case Lookahead (B) is
            when T_WORD =>
               if C = BRACEGROUP_CONTEXT and then
                 Lookahead_Command (B) = T_RBRACE
               then
                  Append (Result, Value);
                  return Result;
               end if;

               Append (Result, Value);
            when others =>
               Append (Result, Value);
               return Result;
         end case;
         Value := Read_Word_Token (B);
      end loop;
   end Parse_Word_List;

end Posix_Shell.Parser;
