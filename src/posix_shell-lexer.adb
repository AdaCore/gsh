with Ada.Exceptions; use Ada.Exceptions;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Subst; use Posix_Shell.Subst;
with Ada.Unchecked_Deallocation;

package body Posix_Shell.Lexer is

   function Get_Char (B : Buffer_Access) return Character;
   pragma Inline (Get_Char);
   --  Get the next char from the buffer and update the buffer state.

   procedure Unget_Char (B : Buffer_Access);
   pragma Inline (Unget_Char);
   --  Undo last Get_Char.

   procedure Pushback (B : Buffer_Access; T : Token);
   pragma Inline (Pushback);
   --  Cancel effect of last Read_Token call.

   function Read_Token_Aux
     (B : Buffer_Access;
      IOHere : Boolean := False)
      return Token;

   function Read_Command_Token_Aux (Tk : Token) return Token;

   procedure Lexer_Error (B : Buffer_Access; Msg : String);
   --  Report an error.

   procedure Deallocate (B : in out Buffer_Access) is
      procedure Internal_Free is
        new Ada.Unchecked_Deallocation
          (Token_Buffer,
           Buffer_Access);
   begin
      B.B := Null_Buffer;
      Internal_Free (B);
   end Deallocate;

   ------------------
   -- Expect_Token --
   ------------------

   procedure Expect_Token
     (B   : Buffer_Access;
      T   : Token_Type;
      Msg : String := "")
   is
      Next : constant Token := Read_Command_Token (B);
   begin
      if Next.T /= T then
         if Msg'Length = 0 then
            Syntax_Error (Next, "expect token '" & Token_Type_Img (T) & "'");
         else
            Syntax_Error (Next, Msg);
         end if;
      end if;
   end Expect_Token;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (B : Buffer_Access) return Character is
      Tmp : constant Character := Current (B.B);
   begin
      Forward (B.B);
      return Tmp;
   end Get_Char;

   --------------------
   -- Get_Token_Type --
   --------------------

   function Get_Token_Type (T : Token) return Token_Type is
   begin
      return T.T;
   end Get_Token_Type;

   ----------------------
   -- Get_Token_String --
   ----------------------

   function Get_Token_String (T : Token) return Annotated_String is
   begin
      return T.S;
   end Get_Token_String;

   -------------------
   -- Get_Token_Pos --
   -------------------

   function Get_Token_Pos (T : Token) return Text_Position is
   begin
      return T.Pos;
   end Get_Token_Pos;

   ------------------
   -- Lexer_Error --
   ------------------

   procedure Lexer_Error (B : Buffer_Access; Msg : String) is
   begin
      Raise_Exception
        (Shell_Lexer_Error'Identity,
         To_String (Current_Lineno (B.B)) &
         ":" & To_String (Current_Columnno (B.B)) & ":" & Msg);
   end Lexer_Error;

   ---------------
   -- Lookahead --
   ---------------

   function Lookahead (B : Buffer_Access) return Token_Type is
      T : constant Token := Read_Token (B);
   begin
      Pushback (B, T);
      return T.T;
   end Lookahead;

   -----------------------
   -- Lookahead_Command --
   -----------------------

   function Lookahead_Command (B : Buffer_Access) return Token_Type is
      --  T : constant Token := Read_Command_Token (B);
      T : constant Token := Read_Command_Token_Aux (Read_Token (B));
   begin
      Pushback (B, T);
      return T.T;
   end Lookahead_Command;

   ----------------
   -- New_Buffer --
   ----------------

   function New_Buffer (Str : String) return Token_Buffer is
      Result : Token_Buffer;
   begin
      Result.B := New_Buffer (Str);
      Result.Previous_Token_Pos := Current_Pos (Result.B);
      Result.Next_Token_Pos := Current_Pos (Result.B);
      Result.Valid_Cache := False;
      return Result;
   end New_Buffer;

   --------------------------
   -- New_Buffer_From_File --
   --------------------------

   function New_Buffer_From_File (Filename : String) return Token_Buffer is
      Result : Token_Buffer;
   begin
      Result.B := New_Buffer_From_File (Filename);
      Result.Previous_Token_Pos := Current_Pos (Result.B);
      Result.Next_Token_Pos := Current_Pos (Result.B);
      Result.Valid_Cache := False;
      return Result;

   end New_Buffer_From_File;

   --------------
   -- Pushback --
   --------------

   procedure Pushback (B : Buffer_Access; T : Token) is
   begin
      B.Next_Token_Pos := Current_Pos (B.B);
      B.Next_Token := T;
      B.Valid_Cache := True;
      Set_Pos (B.B, B.Previous_Token_Pos);
   end Pushback;

   ------------------------
   -- Read_Command_Token --
   ------------------------

   function Read_Command_Token (B : Buffer_Access) return Token is
      T : Token := Read_Token (B);
      Result : constant Token := Read_Command_Token_Aux (T);
   begin
      --  Free unnecessary string
      if Result.T in T_IF .. T_RBRACE then
         T.S := Null_Annotated_String;
      end if;
      return Result;
   end Read_Command_Token;

   function Read_Command_Token (B : Buffer_Access) return Token_Type is
      T : constant Token := Read_Command_Token (B);
   begin
      return T.T;
   end Read_Command_Token;

   ----------------------------
   -- Read_Command_Token_Aux --
   ----------------------------

   function Read_Command_Token_Aux (Tk : Token) return Token is
      T : Token := Tk;

      function New_Token (TT : Token_Type) return Token;
      --  Return a new token of type TT with no string attached

      function Is_Assignment (Expr : String) return Boolean;
      --  Return True if the given expression (Expr) is an assignment.

      -------------------
      -- Is_Assignment --
      -------------------

      function Is_Assignment (Expr : String) return Boolean is
      begin
         for J in Expr'Range loop
            if Expr (J) = '=' then

               --  We have a possible assignment.  Check the name
               --  of the target variable:  If it is a valid name,
               --  then indeed we have an actual assignment. Otherwise,
               --  this equal sign is part of something else, for
               --  instance a quote string, or a command name.

               return Is_Valid_Variable_Name (Expr (Expr'First .. J - 1));
            end if;
         end loop;
         return False;
      end Is_Assignment;

      ---------------
      -- New_Token --
      ---------------

      function New_Token (TT : Token_Type) return Token is
      begin
         return (TT, Null_Annotated_String, T.Pos);
      end New_Token;

   begin
      if T.T = T_WORD then
         declare
            S : constant String := Str (T.S);
         begin
            if S = "if"       then
               T := New_Token (T_IF);
            elsif S = "then"  then
               T := New_Token (T_THEN);
            elsif S = "else"  then
               T := New_Token (T_ELSE);
            elsif S = "elif"  then
               T := New_Token (T_ELIF);
            elsif S = "fi"    then
               T := New_Token (T_FI);
            elsif S = "do"    then
               T := New_Token (T_DO);
            elsif S = "done"  then
               T := New_Token (T_DONE);
            elsif S = "case"  then
               T := New_Token (T_CASE);
            elsif S = "esac"  then
               T := New_Token (T_ESAC);
            elsif S = "while" then
               T := New_Token (T_WHILE);
            elsif S = "until" then
               T := New_Token (T_UNTIL);
            elsif S = "for"   then
               T := New_Token (T_FOR);
            elsif S = "in"    then
               T := New_Token (T_IN);
            elsif S = "{"     then
               T := New_Token (T_LBRACE);
            elsif S = "}"     then
               T := New_Token (T_RBRACE);
            elsif S = "!"     then
               T := New_Token (T_BANG);
            elsif Is_Assignment (S) then
               T.T := T_ASSIGNEMENT;
            end if;
         end;
      end if;

      return T;
   end Read_Command_Token_Aux;

   -----------------
   -- Read_IOHere --
   -----------------

   function Read_IOHere
     (B : Buffer_Access; Marker : Annotated_String)
      return Annotated_String
   is
      CC            : Character;
      Evaluation    : Boolean := True;

      Token_A_String : Annotated_String;

      --  Start_Position : constant Text_Position := Current_Pos (B.B);

      procedure Add_To_Token (C : Character; A : Annotation := NO_ANNOTATION);
      procedure Add_To_Token (T : Token);
      function Return_IOHere return Annotated_String;

      ------------------
      -- Add_To_Token --
      ------------------

      procedure Add_To_Token (C : Character; A : Annotation := NO_ANNOTATION)
      is
      begin
         Append (Token_A_String, C, A);
      end Add_To_Token;

      procedure Add_To_Token (T : Token)
      is
      begin
         Append (Token_A_String, T.S);
      end Add_To_Token;

      function Return_IOHere return Annotated_String
      is
      begin

         return Token_A_String;
      end Return_IOHere;

      Marker_String : constant String := Eval_String_Unsplit
        (null, Marker, Quote_Removal_Only => True);

   begin

      --  Dynamic_Annotations.Init (Token_Annotations);

      for J in 1 .. Length (Marker) loop
         declare
            A : constant Annotation := Get_Annotation (Marker, J);
         begin
            if A = ESCAPE_SEQUENCE or else
              A = SINGLE_QUOTE_BEGIN or else
              A = DOUBLE_QUOTE_BEGIN
            then
               Evaluation := False;
               exit;
            end if;
         end;
      end loop;

      --  First skip everything until new line
      while Get_Char (B) /= ASCII.LF loop null;
      end loop;

      if Current (B.B, Marker_String'Length + 1) =
        Marker_String & ASCII.LF
      then

         --  We have the marker
         CC := Get_Char (B);
         while CC /= ASCII.LF loop
            CC := Get_Char (B);
         end loop;
         Unget_Char (B);
         return Return_IOHere;
      end if;

      loop
         CC := Get_Char (B);
         case CC is
            when ASCII.LF =>
               if Current (B.B, Marker_String'Length + 1) =
                 Marker_String & ASCII.LF
               then

                  --  We have the marker
                  Add_To_Token (ASCII.LF);
                  CC := Get_Char (B);
                  while CC /= ASCII.LF loop

                     CC := Get_Char (B);

                  end loop;
                  Unget_Char (B);
                  return Return_IOHere;
               end if;

               Add_To_Token (CC);
            when ASCII.EOT =>
               Unget_Char (B);
               return Return_IOHere;
               --  Lexer_Error (B, "unexpected EOF in IO Here starting at " &
               --             Image (Start_Position) &
               --            " marked by '" & Marker_String & "'");
            when '\' =>
               if Evaluation then
                  CC := Get_Char (B);
                  case CC is
                     when '$' => Add_To_Token ('$', ESCAPE_SEQUENCE);
                     when '`' => Add_To_Token ('`', ESCAPE_SEQUENCE);
                     when '\' => Add_To_Token ('\', ESCAPE_SEQUENCE);
                     when others => Add_To_Token ('\'); Unget_Char (B);
                  end case;
               else
                  Add_To_Token (CC);
               end if;
            when '$' | '`' =>
               if Evaluation then
                  Unget_Char (B);
                  declare
                     T : constant Token := Read_Token_Aux (B, True);
                  begin
                     Add_To_Token (T);
                  end;
               else
                  Add_To_Token (CC);
               end if;
            when others =>
               Add_To_Token (CC);
         end case;

      end loop;
   end Read_IOHere;

   ----------------
   -- Read_Token --
   ----------------

   function Read_Token (B : Buffer_Access) return Token is
   begin
      if B.Valid_Cache then
         Set_Pos (B.B, B.Next_Token_Pos);
         B.Valid_Cache := False;
         return B.Next_Token;
      else
         return Read_Token_Aux (B);
      end if;
   end Read_Token;

   function Read_Token (B : Buffer_Access) return Token_Type is
      T : constant Token := Read_Token (B);
   begin
      return T.T;
   end Read_Token;

   function Read_Token (B : Buffer_Access) return Annotated_String is
      T : constant Token := Read_Token (B);
   begin
      return T.S;
   end Read_Token;

   --------------------
   -- Read_Token_Aux --
   --------------------

   function Read_Token_Aux (B : Buffer_Access;
                            IOHere : Boolean := False)
                            return Token
   is

      Token_A_String     : Annotated_String;
      --  Buffer containing the current token chars

      Has_Token        : Boolean := False;
      --  True if token recognition has started

      --  Current_Line     : constant Integer := Current_Lineno (B.B);
      --  Current_Column   : Integer := Current_Columnno (B.B);
      Pos : Text_Position := Current_Pos (B.B);

      CC               : Character;

      function Return_Token
        (T : Token_Type    := T_WORD;
         S : Boolean       := False)
         return Token;
      pragma Inline (Return_Token);

      function Delimit_Token (T : Token_Type := T_WORD) return Token;
      pragma Inline (Delimit_Token);

      procedure Add_To_Token (C : Character; A : Annotation := NO_ANNOTATION);
      pragma Inline (Add_To_Token);

      procedure Read_Escape_Sequence
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean);

      procedure Read_Single_Quote (Skip_Mode : Boolean);
      procedure Read_Double_Quote (Skip_Mode : Boolean);

      procedure Read_Command_Substitution
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean);
      procedure Read_Backquote_Command_Substitution
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean);
      procedure Read_Parameter_Evaluation
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean);

      ------------------
      -- Add_To_Token --
      ------------------

      procedure Add_To_Token (C : Character; A : Annotation := NO_ANNOTATION)
      is
      begin
         Append (Token_A_String, C, A);
      end Add_To_Token;

      -------------------
      -- Delimit_Token --
      -------------------

      function Delimit_Token (T : Token_Type := T_WORD) return Token is
      begin
         Unget_Char (B);
         return Return_Token (T, S => True);
      end Delimit_Token;

      -------------------------------
      -- Read_Command_Substitution --
      -------------------------------

      procedure Read_Backquote_Command_Substitution
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean)
      is
         Begin_Marker : Annotation := COMMAND_SUBST_BEGIN;
         End_Marker   : Annotation := COMMAND_SUBST_END;
      begin
         Has_Token := True;

         if Skip_Mode then
            Begin_Marker := NO_ANNOTATION;
            End_Marker   := NO_ANNOTATION;
         end if;

         Add_To_Token (CC, Begin_Marker);

         loop
            CC := Get_Char (B);
            case CC is
               when '\' =>
                  CC := Get_Char (B);
                  case CC is
                     when '`' | '$' | '\' =>
                        Add_To_Token (CC);
                     when ASCII.LF =>
                        null;
                     when others =>
                        if In_Double_Quote and CC = '"' then
                           Add_To_Token (CC);
                        else
                           Add_To_Token ('\');
                           Add_To_Token (CC);
                        end if;
                  end case;
               when '`' =>
                  Add_To_Token (CC, End_Marker);
                  exit;
               when others =>
                  Add_To_Token (CC);
            end case;
         end loop;
      end Read_Backquote_Command_Substitution;


      procedure Read_Command_Substitution
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean)
      is
         Begin_Marker : Annotation := COMMAND_SUBST_BEGIN;
         End_Marker : Annotation := COMMAND_SUBST_END;
      begin
         pragma Assert (CC = '$');
         Has_Token := True;

         if Skip_Mode then
            Begin_Marker := NO_ANNOTATION;
            End_Marker := NO_ANNOTATION;
         end if;

         CC := Get_Char (B);

         if CC /= '(' then
            Add_To_Token ('$');
            Unget_Char (B);
            return;
         end if;

         Add_To_Token ('$', Begin_Marker);
         Add_To_Token (CC);

         loop
            CC := Get_Char (B);
            case CC is
               when '\' => Read_Escape_Sequence
                    (Skip_Mode => True, In_Double_Quote => False);
               when '`' => Read_Backquote_Command_Substitution
                    (Skip_Mode => True, In_Double_Quote => In_Double_Quote);
               when '$' => Read_Parameter_Evaluation
                    (Skip_Mode => True, In_Double_Quote => In_Double_Quote);
               when '"' => Read_Double_Quote
                    (Skip_Mode => True);
               when ''' => Read_Single_Quote
                    (Skip_Mode => True);
               when '#' =>
                  while Get_Char (B) /= ASCII.LF loop null; end loop;
                  Unget_Char (B);
               when ')' =>
                  Add_To_Token (CC, End_Marker);
                  exit;
               when others => Add_To_Token (CC);
            end case;
         end loop;
      end Read_Command_Substitution;

      -----------------------
      -- Read_Double_Quote --
      -----------------------

      procedure Read_Double_Quote (Skip_Mode : Boolean) is
         Begin_Marker : Annotation := DOUBLE_QUOTE_BEGIN;
         End_Marker   : Annotation := DOUBLE_QUOTE_END;
      begin
         if Skip_Mode then
            Begin_Marker := NO_ANNOTATION;
            End_Marker := NO_ANNOTATION;
         end if;

         Has_Token := True;
         Add_To_Token (CC, Begin_Marker);

         loop
            CC := Get_Char (B);
            if CC = '"' then
               Add_To_Token (CC, End_Marker);
               exit;
            else
               case CC is
                  when '\' => Read_Escape_Sequence
                       (Skip_Mode => Skip_Mode, In_Double_Quote => True);
                  when '`' => Read_Backquote_Command_Substitution
                       (In_Double_Quote => True, Skip_Mode => Skip_Mode);
                  when '$' => Read_Parameter_Evaluation
                       (In_Double_Quote => True, Skip_Mode => Skip_Mode);
                  when ASCII.EOT =>
                     Lexer_Error
                       (B, "unexpected EOF while looking for matchin '""'");
                  when others => Add_To_Token (CC);
               end case;
            end if;
         end loop;
      end Read_Double_Quote;

      --------------------------
      -- Read_Escape_Sequence --
      --------------------------

      procedure Read_Escape_Sequence
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean)
      is
      begin
         --  Section 2.2.1 Escape Character (\)
         --  A backslash that is not quoted shall preserve the literal
         --  value of the following character, with the exception of a
         --  <newline>. If a <newline> follows the backslash, the shell
         --  shall interpret this as line continuation. The backslash and
         --  <newline>s shall be removed before splitting the input into
         --  tokens. Since the escaped <newline> is removed entirely from
         --  the input and is not replaced by any white space, it cannot
         --  serve as a token separator.

         CC := Get_Char (B);
         if CC /= ASCII.LF then
            --  This also refer implicitely to Section 2.3 Rule 11
            Has_Token := True;
            if not Skip_Mode then
               if not In_Double_Quote or else CC = '"' or else CC = '`' or else
                 CC = '$' or else CC = '\'
               then
                  Add_To_Token (CC, ESCAPE_SEQUENCE);
               else
                  Add_To_Token ('\');
                  Unget_Char (B);
               end if;
            else
                  Add_To_Token ('\');
                  Add_To_Token (CC);
            end if;
            --  Add_To_Token (CC);
         end if;
      end Read_Escape_Sequence;

      -------------------------------
      -- Read_Parameter_Evaluation --
      -------------------------------

      procedure Read_Parameter_Evaluation
        (Skip_Mode       : Boolean;
         In_Double_Quote : Boolean)
      is

         Begin_Marker : Annotation := PARAM_EVAL_BEGIN;
         End_Marker : Annotation   := PARAM_EVAL_END;

      begin
         if Skip_Mode then
            Begin_Marker := NO_ANNOTATION;
            End_Marker := NO_ANNOTATION;
         end if;

         Has_Token := True;
         CC := Get_Char (B);

         --  If the expression starts with a '(', then it is actually
         --  not a parameter expression, but rather a command expression.

         if CC = '(' then
            Unget_Char (B);
            CC := '$';
            Read_Command_Substitution
              (Skip_Mode => Skip_Mode, In_Double_Quote => False);
            return;
         end if;

         --  Check the first character following the '$' sign.
         --  If it is a paramater evaluation, then only a few selected
         --  characters can follow it.  Otherwise, the $ sign is just
         --  a token on its own.

         if not (CC in 'a' .. 'z'
                 or else CC in 'A' .. 'Z'
                 or else CC in '0' .. '9'
                 or else CC = '{'
                 or else CC = '_'
                 or else CC = '@'
                 or else CC = '*'
                 or else CC = '#'
                 or else CC = '?'
                 or else CC = '-'
                 or else CC = '$'
                 or else CC = '!')
         then
            Add_To_Token ('$');
            Unget_Char (B);
            return;
         end if;

         --  This parameter is indeed the first character of a parameter
         --  name, and thus the '$' sign is indeed the start of a parameter
         --  evaluation.

         Add_To_Token ('$', Begin_Marker);

         --  If the '$' is followed by '{', then the parameter evaluation
         --  expression ends at the associated enclosing '}'.

         if CC = '{' then
            while CC /= '}'
            loop
               if CC = '\' then
                  Read_Escape_Sequence
                    (Skip_Mode => Skip_Mode, In_Double_Quote => False);
               elsif CC = '"' then
                  Read_Double_Quote (Skip_Mode => Skip_Mode);
               elsif CC = ''' then
                  Read_Single_Quote (Skip_Mode => Skip_Mode);
               elsif CC = '`' then
                  Read_Backquote_Command_Substitution
                    (Skip_Mode => Skip_Mode, In_Double_Quote => False);
               elsif CC = '$' then
                  Read_Parameter_Evaluation
                    (Skip_Mode => Skip_Mode, In_Double_Quote => False);
               else
                  Add_To_Token (CC);
               end if;
               CC := Get_Char (B);
            end loop;

            Add_To_Token (CC, End_Marker);
            return;
         end if;

         --  Is this a positional or special parameter? In such case,
         --  the token ends at the first character.

         if CC in '0' .. '9' --  Positional parameter
           or else CC = '@'  --  Special parameter
           or else CC = '*'  --  Special parameter
           or else CC = '#'  --  Special parameter
           or else CC = '?'  --  Special parameter
           or else CC = '-'  --  Special parameter
           or else CC = '$'  --  Special parameter
           or else CC = '!'  --  Special parameter
         then
            Add_To_Token (CC, End_Marker);
            return;
         end if;

         --  This is a more usual parameter. The token ends when we find
         --  a character that is not valid in a parameter name. Then,
         --  annotate the last valid character as the end of the parameter.

         declare
            Next_Char : Character;
         begin
            loop
               Next_Char := Get_Char (B);
               case Next_Char is
                  when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
                     Add_To_Token (CC);
                  when others =>
                     Add_To_Token (CC, End_Marker);
                     Unget_Char (B);
                     return;
               end case;
               CC := Next_Char;
            end loop;
         end;

      end Read_Parameter_Evaluation;

      -----------------------
      -- Read_Single_Quote --
      -----------------------

      procedure Read_Single_Quote (Skip_Mode : Boolean) is
         Start_Line   : constant Integer := Current_Lineno (B.B);
         Start_Column : constant Integer := Current_Columnno (B.B);
         Begin_Marker : Annotation := SINGLE_QUOTE_BEGIN;
         End_Marker   : Annotation := SINGLE_QUOTE_END;
      begin
         if Skip_Mode then
            Begin_Marker := NO_ANNOTATION;
            End_Marker   := NO_ANNOTATION;
         end if;

         Has_Token := True;

         Add_To_Token (CC, Begin_Marker);

         loop
            CC := Get_Char (B);

            case CC is
               when ''' =>
                  Add_To_Token (CC, End_Marker);
                  exit;
               when ASCII.EOT =>
                  Lexer_Error
                    (B,
                     "unexpected EOF in single quote starting at " &
                     To_String (Start_Line) &
                     ":" & To_String (Start_Column));
               when others =>
                  Add_To_Token (CC);
            end case;
         end loop;
      end Read_Single_Quote;

      ------------------
      -- Return_Token --
      ------------------

      function Return_Token
        (T : Token_Type    := T_WORD;
         S : Boolean       := False)
         return Token
      is
      begin
         if S then
            --  Ada.Text_IO.Put_Line (Image (Token_A_String));
            return (T, Token_A_String, Pos);
         end if;
         return (T, Null_Annotated_String, Pos);
      end Return_Token;

   begin
      --  Save current state of the buffer in case pushback is called afterward
      B.Previous_Token_Pos      := Current_Pos (B.B);
      loop
         CC := Get_Char (B);

         --  Check if we should delimit the token
         if Has_Token then
            case CC is
               when ASCII.EOT | '(' | ')' | ';' | '&' | '|' | ASCII.LF | ' ' |
                    ASCII.HT =>
                  return Delimit_Token;
               when '>' | '<' =>
                  if Is_Natural (Str (Token_A_String)) then
                     return Delimit_Token (T_IO_NUMBER);
                  else
                     return Delimit_Token;
                  end if;
               when others => null;
            end case;
         end if;


         --  [Section 2.3 Rule 2]
         --  If the previous character was used as part of an operator and the
         --  current character is not quoted and can be used with the current
         --  characters to form an operator, it shall be used as part of that
         --  (operator) token.

         --  [Section 2.3 Rule 3]
         --  If the previous character was used as part of an operator and the
         --  current character cannot be used with the current characters to
         --  form an operator, the operator containing the previous character
         --  shall be delimited.

         --  [Section 2.3 Rule 6]
         --  If the current character is not quoted and can be used as the
         --  first character of a new operator, the current token (if any)
         --  shall be delimited. The current character shall be used as the
         --  beginning of the next (operator) token.

         case CC is
            when ASCII.EOT =>
               --  If there is no current token, the end-of-input indicator
               --  shall be returned as the token.
               return Return_Token (T_EOF);
            when '(' =>
               --  [Section 2.3 Rule 3]
               return Return_Token (T_LPAR);

            when ')' =>
               --  [Section 2.3 Rule 3]
               return Return_Token (T_RPAR);

            when ';' =>
               if Get_Char (B) = ';' then
                  --  [Section 2.3 Rule 2]
                  --  [Section 2.3 Rule 3]
                  return Return_Token (T_DSEMI);
               else
                  Unget_Char (B);
                  --  [Section 2.3 Rule 3]
                  return Return_Token (T_SEMI);
               end if;

            when '&' =>

               if Get_Char (B) = '&' then
                  --  [Section 2.3 Rule 2]
                  --  [Section 2.3 Rule 3]
                  return Return_Token (T_AND_IF);
               else
                  Unget_Char (B);
                  --  [Section 2.3 Rule 3]
                  return Return_Token (T_AND);
               end if;

            when '|' =>

               if Get_Char (B) = '|' then
                  return Return_Token (T_OR_IF);
               else
                  Unget_Char (B);
                  return Return_Token (T_PIPE);
               end if;

            when '>' =>

               CC := Get_Char (B);
               case CC is
                  when '>' =>
                     return Return_Token (T_DGREAT);
                  when '|' =>
                     return Return_Token (T_CLOBBER);
                  when '&' =>
                     return Return_Token (T_GREATAND);
                  when others =>
                     Unget_Char (B);
                     return Return_Token (T_GREAT);
               end case;

            when '<' =>
               CC := Get_Char (B);
               case CC is
                  when '<' =>
                     if Get_Char (B) = '-' then
                        return Return_Token (T_DLESSDASH);
                     else
                        Unget_Char (B);
                        return Return_Token (T_DLESS);
                     end if;
                  when '&' =>
                     return Return_Token (T_LESSAND);
                  when '>' =>
                     return Return_Token (T_LESSGREAT);
                  when others =>
                     Unget_Char (B);
                     return Return_Token (T_LESS);
               end case;
            when ASCII.LF =>
               return Return_Token (T_NEWLINE);
            when ' ' | ASCII.HT =>
               --  discard <blank> character
               Pos := Current_Pos (B.B);
            when '\' =>
               Read_Escape_Sequence (False, False);
            when '"' =>
               Read_Double_Quote (False);
            when ''' =>
               Read_Single_Quote (False);
            when '`' =>
               Read_Backquote_Command_Substitution (False, False);
               if IOHere then
                  return Return_Token (T_WORD, S => True);
               end if;
            when '$' =>
               Read_Parameter_Evaluation (False, False);
               if IOHere then
                  return Return_Token (T_WORD, S => True);
               end if;
            when others =>
               if CC = '#' and then not Has_Token then
                  --  [Section 2.3. Rule 10]
                  --  If the current character is a '#', it and all subsequent
                  --  characters up to, but excluding, the next <newline> shall
                  --  be discarded as a comment. The <newline> that ends the
                  --  line is not considered part of the comment.

                  declare
                     Tmp_CC : Character := Get_Char (B);
                  begin
                     while Tmp_CC /= ASCII.LF and Tmp_CC /= ASCII.EOT loop
                        Tmp_CC := Get_Char (B);

                     end loop;
                  end;
                  Unget_Char (B);
               else
                  --  [Section 2.3 Rule 11]
                  --  The current character is used as the start of a new word.
                  Has_Token := True;

                  --  [Section 2.3 Rule 9]
                  --  If the previous character was part of a word, the current
                  --  character shall be appended to that word.
                  Add_To_Token (CC);
               end if;
         end case;

      end loop;
   end Read_Token_Aux;

   ---------------------
   -- Read_Word_Token --
   ---------------------

   function Read_Word_Token (B : Buffer_Access) return Annotated_String is
      Next : constant Token := Read_Token (B);
   begin
      if Next.T /= T_WORD then
         Syntax_Error (Next, "expect word token");
      end if;
      return Next.S;
   end Read_Word_Token;

   ----------------
   -- Skip_Token --
   ----------------

   procedure Skip_Token (B : Buffer_Access) is
      T : Token := Read_Token (B);
      pragma Unreferenced (T);
   begin
      null;
   end Skip_Token;

   ------------------
   -- Syntax_Error --
   ------------------

   procedure Syntax_Error (T : Token; Msg : String) is
   begin
      if T.T = T_WORD then
         Raise_Exception
           (Shell_Syntax_Error'Identity,
            Image (T.Pos) & ":"
            & Msg & " (got '"
            & Token_Type_Img (T.T) & "'," & Str (T.S) & ")");
      else
         Raise_Exception
           (Shell_Syntax_Error'Identity,
            Image (T.Pos) & ":"
            & Msg & " (got '"
            & Token_Type_Img (T.T) & "')");
      end if;

   end Syntax_Error;

   function Token_Pos_Img (T : Token) return String is
   begin
      return Image (T.Pos) & "-> " & Token_Type_Img (T.T);
   end Token_Pos_Img;

   --------------------
   -- Token_Type_Img --
   --------------------

   function Token_Type_Img (T : Token_Type) return String is
   begin
      case T is
         when T_WORD        => return "word";
         when T_EOF         => return "end of file";
         when T_ASSIGNEMENT => return "assignement";
         when T_DSEMI       => return ";;";
         when T_AND_IF      => return "&&";
         when T_AND         => return "&";
         when T_OR_IF       => return "||";
         when T_PIPE        => return "|";
         when T_DGREAT      => return ">>";
         when T_CLOBBER     => return ">|";
         when T_GREATAND    => return ">&";
         when T_DLESSDASH   => return ">>-";
         when T_LESS        => return "<";
         when T_GREAT       => return ">";
         when T_DLESS       => return "<<";
         when T_LESSAND     => return "<&";
         when T_LESSGREAT   => return "<>";
         when T_NEWLINE     => return "<LF>";
         when T_LPAR        => return "(";
         when T_SEMI        => return ";";
         when T_RPAR        => return ")";
         when T_IF          => return "if";
         when T_THEN        => return "then";
         when T_ELSE        => return "else";
         when T_ELIF        => return "elif";
         when T_FI          => return "fi";
         when T_DO          => return "do";
         when T_DONE        => return "done";
         when T_BANG        => return "!";
         when T_IN          => return "in";
         when T_CASE        => return "case";
         when T_ESAC        => return "esac";
         when T_WHILE       => return "while";
         when T_UNTIL       => return "until";
         when T_FOR         => return "for";
         when T_LBRACE      => return "{";
         when T_RBRACE      => return "}";
         when others        => return T'Img;
      end case;
   end Token_Type_Img;

   ----------------
   -- Unget_Char --
   ----------------

   procedure Unget_Char (B : Buffer_Access) is
   begin
      Rewind (B.B);
   end Unget_Char;

end Posix_Shell.Lexer;
