------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                         Sh.Subst.Arith                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2019, AdaCore                   --
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

with GNAT.Regpat;                  use GNAT.Regpat;

with Sh.Subst.Arith;      use Sh.Subst.Arith;
with Sh.String_Utils;     use Sh.String_Utils;
with Sh.Utils;            use Sh.Utils;

with Sh.States.IO; use Sh.States.IO;

package body Sh.Subst.Arith is

   --  here is the list of operators supported by the expr utility:
   --   |  &  =  >  >=  <  <=  !=  +  -  *  /  %  :
   --  they are ordered by increasing priority
   --
   --  the expr utility support also parenthesis

   type Expr_Token_Type is
     (EOC,           --            | lowest priority operator
      RIGHT_PAR,     -- ')'        |
      PIPE,          -- '|'        |
      CAND,          -- '&'        |
      EQUAL,         -- '='        | when Is_Arith_Exp affectation
                     --            | otherwise eq test
      DOUBLE_EQUAL,  -- '=='       | only when Is_Arith_Exp
      GT,            -- '>'        |
      GE,            -- '>='       |
      LT,            -- '<'        |
      LE,            -- '<='       |
      DIFF,          -- '!='       |
      PLUS,          -- '+'        |
      UNARY_PLUS,    -- '-'        |
      DOUBLE_PLUS,   -- '++'       | only when Is_Arith_Exp
      MINUS,         -- '-'        |
      UNARY_MINUS,   -- '-'        |
      DOUBLE_MINUS,  -- '--'       | only when Is_Arith_Exp
      STAR,          -- '*'        |
      DIV,           -- '/'        |
      PERCENT,       -- '%'        |
      COLON,         -- ':'        |
      LEFT_PAR,      -- '('        V highest priority operator
      NUM,
      STRG);         -- numbers

   type Expr_Token is record
      T        : Expr_Token_Type;
      I        : Integer;
      S        : String_Access;
      Var_Name : String_Access := null;
   end record;
   --  expr tokens can be eitheir an operator or a number. When T field is NUM
   --  then I field contains the number value.

   Null_Expr_Token : constant Expr_Token := (EOC, 0, null, null);

   -----------------
   --  Eval_Expr  --
   -----------------

   function Eval_Expr (S            : in out Shell_State;
                       Args         : String_List;
                       Is_Arith_Exp : Boolean := False) return String
   is

      Index : Integer := Args'First;
      --  index of the current token

      Op_Stack     : array (1 .. Args'Length) of Expr_Token :=
                       (others => Null_Expr_Token);
      Arg_Stack    : array (1 .. Args'Length) of Expr_Token;

      --  stack containing the temp results and pending ops. It cannot be
      --  bigger than the number of arguments passed to expr

      Arg_Stack_Top : Integer := 0;
      Op_Stack_Top  : Integer := 0;
      --  Index of top of the stack for numbers and operators.

      --  the algorithm uses two stacks: one for the operations and one for the
      --  numbers. At the beginning both stacks are empty.
      --
      --  The procedure iterate through the token list and do the following
      --  action for the current token (CT):
      --   * if CT is a number then push the number on Stack_Number
      --   * if CT is an operator:
      --       1- do all the operations on the stack op for which priority
      --       is superior or equal to the current op. Result is push on the
      --       number stack
      --       2- push the operator on the operator stack
      --
      --  The EOC operator has the lowest priority and is emit by Next to force
      --  evaluation of the remaining ops on the stack.
      --
      --  Parenthesis are handled in special way. Need to document
      --
      --  Ex : expr 2 + 3 * 5 + 4
      --  It 0         It 2         It 4         It 6       It 8
      --  |  |  |push +|  |  |push *|  |  |3*5   |  |  |17+4|  |  |
      --  |  |  |      |  |  |      |  |  |17+2  |  |  |    |  |  |
      --  |  |  |----->|  |  |----->|3 |* |push +|  |  |--->|  |  |
      --  |  |  |      |2 |+ |      |2 |+ |----->|17|+ |    |21|  |
      --  |SN|SO|      |SN|SO|      |SN|SO|      |SN|SO|    |SN|SO|
      --

      CT : Expr_Token := Null_Expr_Token; --  Current token
      PT : Expr_Token; --  Previous token

      function Next return Expr_Token;
      procedure Push_Result (I : Integer; One_Arg : Boolean := False);
      procedure Push_Result (S : String);
      procedure Push_Result (T : Expr_Token);
      procedure Check_Type (L, R : Expr_Token; T : Expr_Token_Type);

      ----------------
      -- Check_Type --
      ----------------

      procedure Check_Type (L, R : Expr_Token; T : Expr_Token_Type) is
      begin
         if L.T /= T or else R.T /= T then
            raise Expr_Error;
         end if;

      end Check_Type;

      ----------
      -- Next --
      ----------

      function Next return Expr_Token
      is
         Str         : constant String := Args (Index).all;
         Str_Length  : constant Integer := Str'Length;
         N           : Integer := 0;
         Is_Num      : Boolean := False;

         function Return_Op_Token
           (T            : Expr_Token_Type;
            Second_Char  : Character := ASCII.NUL;
            First_Choice : Expr_Token_Type := EOC)
            return Expr_Token;

         function Return_Op_Token
           (T            : Expr_Token_Type;
            Second_Char  : Character := ASCII.NUL;
            First_Choice : Expr_Token_Type := EOC)
            return Expr_Token
         is
         begin
            if Str_Length = 1 then
               if Second_Char = ASCII.NUL then
                  return (T, 0, null, null);
               elsif First_Choice /= EOC then
                  return (First_Choice, 0, null, null);
               else
                  return (STRG, 0, new String'(Str), null);
               end if;
            elsif Str_Length = 2 and then
              Second_Char = Str (Str'First + 1)
            then
               return (T, 0, null, null);
            else
               return (STRG, 1, new String'(Str), null);
            end if;
         end Return_Op_Token;

      begin
         Index := Index + 1;

         if Str_Length = 0 then
            return (STRG, 0, new String'(""), null);
         end if;

         case Str (Str'First) is
            when ')' => return Return_Op_Token (RIGHT_PAR);
            when '(' => return Return_Op_Token (LEFT_PAR);
            when '|' => return Return_Op_Token (PIPE);
            when '&' => return Return_Op_Token (CAND);
            when '=' => return Return_Op_Token (DOUBLE_EQUAL, '=', EQUAL);
            when '>' => return Return_Op_Token (GE, '=', GT);
            when '<' => return Return_Op_Token (LE, '=', LT);
            when '!' => return Return_Op_Token (DIFF, '=');
            when '*' => return Return_Op_Token (STAR);
            when '/' => return Return_Op_Token (DIV);
            when '%' => return Return_Op_Token (PERCENT);
            when ':' => return Return_Op_Token (COLON);

            when '+' =>
               if not Is_Arith_Exp then
                  return Return_Op_Token (PLUS);
               else
                  if Str_Length = 1 then
                     case PT.T is
                        when NUM | STRG | RIGHT_PAR =>
                           return (PLUS, 0, null, null);

                        when others =>
                           return (UNARY_PLUS, 0, null, null);
                     end case;

                  elsif Str = "++" then
                     return (DOUBLE_PLUS, 0, null, null);
                  else
                     return Return_Op_Token (PLUS);
                  end if;
               end if;

            when '-' =>

               if not Is_Arith_Exp then
                  if Str_Length = 1 then
                     return (MINUS, 0, null, null);
                  end if;
               else
                  if Str_Length = 1 then
                     case PT.T is
                        when NUM | STRG | RIGHT_PAR =>
                           return (MINUS, 0, null, null);

                        when others =>
                           return (UNARY_MINUS, 0, null, null);
                     end case;

                  elsif Str = "--" then
                     return (DOUBLE_MINUS, 0, null, null);
                  end if;
               end if;

               --  this is a negative number.
               To_Integer (Str, N, Is_Num);

               if Is_Num then
                  return (NUM, N, new String'(Str), null);
               else
                  return (STRG, 1, new String'(Str), null);
               end if;

            when '0' .. '9' =>

               To_Integer (Str, N, Is_Num);
               if Is_Num then
                  return (NUM, N, new String'(Str), null);
               else
                  return (STRG, 1, new String'(Str), null);
               end if;

            when 'a' .. 'z' | 'A' .. 'Z' | '_' =>

               if Is_Arith_Exp then

                  declare
                     Var_Value           : constant String :=
                                             Get_Var_Value (S, Str);
                     Tokenized_List      : CList;
                     Previous_Was_Number : Boolean := False;
                  begin

                     if Var_Value = "" then
                        --  variable is not defined
                        --  use 0 as value
                        return (NUM,
                                0,
                                new String'("0"),
                                new String'(Str));
                     end if;

                     To_Integer (Var_Value, N, Is_Num);

                     if Is_Num then
                        --  the substitution of the variable directly leads
                        --  to a value.
                        return (NUM,
                                N,
                                new String'(Var_Value),
                                new String'(Str));
                     else
                        --  the substitution retuns a string that must be
                        --  a valid arithmetic expression.
                        Split_Arithmetic_String (S,
                                                 Var_Value,
                                                 Previous_Was_Number,
                                                 Tokenized_List);
                        declare
                           --  the expression is evaluated and then returned
                           Value : constant String :=
                                     Eval_Expr (S,
                                                As_List (Tokenized_List),
                                                True);
                        begin
                           To_Integer (Value, N, Is_Num);

                           if Is_Num then
                              return (NUM,
                                      N,
                                      new String'(Value),
                                      new String'(Str));
                           else
                              return (STRG,
                                      1,
                                      new String'(Var_Value),
                                      new String'(Str));
                           end if;
                        end;
                     end if;
                  end;

               else
                  return (STRG, 1, new String'(Str), null);
               end if;

            when others => return (STRG, 1, new String'(Str), null);
         end case;

      end Next;

      -----------------
      -- Push_Result --
      -----------------

      procedure Push_Result (I : Integer; One_Arg : Boolean := False) is
      begin

         if Arg_Stack (Arg_Stack_Top).S /= null then
            Free (Arg_Stack (Arg_Stack_Top).S);
         end if;
         if Arg_Stack (Arg_Stack_Top).Var_Name /= null then
            Free (Arg_Stack (Arg_Stack_Top).Var_Name);
         end if;

         if not One_Arg then
            if Arg_Stack (Arg_Stack_Top - 1).S /= null then
               Free (Arg_Stack (Arg_Stack_Top - 1).S);
            end if;
            if Arg_Stack (Arg_Stack_Top - 1).Var_Name /= null then
               Free (Arg_Stack (Arg_Stack_Top - 1).Var_Name);
            end if;
            --  remove the seconde argument when necessary
            Arg_Stack_Top := Arg_Stack_Top - 1;
         end if;

         --  push the result
         Arg_Stack (Arg_Stack_Top) := (NUM,
                                       I,
                                       new String'(To_String (I)),
                                       null);
      end Push_Result;

      -----------------
      -- Push_Result --
      -----------------

      procedure Push_Result (S : String) is
      begin
         Arg_Stack_Top := Arg_Stack_Top - 1;
         if S = "" then
            Arg_Stack (Arg_Stack_Top) := (STRG, 0, new String'(S), null);
         else
            Arg_Stack (Arg_Stack_Top) := (STRG, 0, new String'(S), null);
         end if;
      end Push_Result;

      -----------------
      -- Push_Result --
      -----------------

      procedure Push_Result (T : Expr_Token) is
      begin
         if T.S /= Arg_Stack (Arg_Stack_Top).S then
            if Arg_Stack (Arg_Stack_Top).S /= null then
               Free (Arg_Stack (Arg_Stack_Top).S);
            end if;
            if Arg_Stack (Arg_Stack_Top).Var_Name /= null then

               Free (Arg_Stack (Arg_Stack_Top).Var_Name);
            end if;
         end if;

         if T.S /= Arg_Stack (Arg_Stack_Top - 1).S then
            if Arg_Stack (Arg_Stack_Top - 1).S /= null then
               Free (Arg_Stack (Arg_Stack_Top - 1).S);
            end if;
            if Arg_Stack (Arg_Stack_Top - 1).Var_Name /= null then

               Free (Arg_Stack (Arg_Stack_Top - 1).Var_Name);
            end if;
         end if;

         Arg_Stack_Top := Arg_Stack_Top - 1;

         --  push the result
         Arg_Stack (Arg_Stack_Top) := T;
      end Push_Result;

      Op          : Expr_Token_Type;
      Left, Right : Expr_Token;
      Opened_Par  : Natural := 0;
   begin

      loop
         exit when Index > Args'Last + 1;

         if Index = Args'Last + 1 then
            --  We have reached the end of the expression passed as argument of
            --  expr so force evaluation of the remaining op in the stack
            PT := CT;
            CT := Null_Expr_Token;
            Index := Index + 1;
         else
            PT := CT;
            CT := Next;
         end if;

         case CT.T is

            when NUM =>

               Arg_Stack_Top := Arg_Stack_Top + 1;
               Arg_Stack (Arg_Stack_Top) := CT;

            when STRG =>

               if Is_Arith_Exp then
                  if CT.Var_Name /= null then
                     Error (S,
                            "A valid arithmetic expression is expected as"
                            &  " value for the variable "
                            & CT.Var_Name.all
                            & ": " & CT.S.all & " is not valid");
                  else
                     Error (S,
                            "A valid arithmetic expression is expected : "
                            & CT.S.all & " is not valid");
                  end if;

                  raise Expr_Error;
               else

                  Arg_Stack_Top := Arg_Stack_Top + 1;
                  Arg_Stack (Arg_Stack_Top) := CT;
               end if;

            when UNARY_PLUS =>
               null;

            when DOUBLE_PLUS | DOUBLE_MINUS =>
               if Is_Arith_Exp then
                  Error (S,
                         "++ and -- operations are currently not"
                         &  " supported in arith expr ");
                  raise Expr_Error;
               else
                  raise Expr_Error;
               end if;

            when others =>

               while Op_Stack_Top /= 0
                 and then CT.T <= Op_Stack (Op_Stack_Top).T
               loop
                  Op := Op_Stack (Op_Stack_Top).T;
                  Op_Stack_Top := Op_Stack_Top - 1;

                  exit when Op = RIGHT_PAR;

                  if Arg_Stack_Top > 1 then

                     Right := Arg_Stack (Arg_Stack_Top);
                     Left  := Arg_Stack (Arg_Stack_Top - 1);

                     case Op is

                        when UNARY_MINUS =>
                           if not Is_Arith_Exp or else Right.T /= NUM then
                              raise Expr_Error;
                           end if;
                           Push_Result (-Right.I,
                                        One_Arg => True);

                        when PLUS  =>
                           Check_Type (Left, Right, NUM);
                           Push_Result (Left.I + Right.I);

                        when MINUS =>
                           Check_Type (Left, Right, NUM);
                           Push_Result (Left.I - Right.I);

                        when STAR  =>
                           Check_Type (Left, Right, NUM);
                           Push_Result (Left.I * Right.I);

                        when DIV   =>
                           Check_Type (Left, Right, NUM);
                           if Right.I = 0 then
                              raise Expr_Error;
                           end if;
                           Push_Result (Left.I / Right.I);

                        when PERCENT =>
                           Check_Type (Left, Right, NUM);
                           if Right.I = 0 then
                              raise Expr_Error;
                           end if;
                           Push_Result (Left.I - (Left.I / Right.I) * Right.I);

                        when PIPE =>
                           if Left.I = 0 then
                              if Right.I = 0 then
                                 Push_Result (0);
                              else
                                 Push_Result (Right);
                              end if;
                           else
                              Push_Result (Left);
                           end if;

                        when CAND =>
                           if Left.I /= 0 and Right.I /= 0 then
                              Push_Result (Left);
                           else
                              Push_Result (0);
                           end if;

                        when EQUAL =>

                           if Is_Arith_Exp then
                              --  in the case of arithmetic expansion
                              --  this operator introduce an affectation.
                              if Right.T /= NUM then
                                 raise Expr_Error;
                              end if;

                              if Left.Var_Name = null then
                                 Error (S,
                                        "affectation expects variable symbole:"
                                        & " '" & Left.S.all & "'");
                                 raise Expr_Error;
                              end if;

                              Set_Var_Value (S,
                                             Left.Var_Name.all,
                                             Right.S.all);

                              Push_Result (Right.I);
                           else
                              if Left.T = NUM and Right.T = NUM then
                                 if Left.I = Right.I then
                                    Push_Result (1);
                                 else
                                    Push_Result (0);
                                 end if;
                              else
                                 if Left.S.all = Right.S.all then
                                    Push_Result (1);
                                 else
                                    Push_Result (0);
                                 end if;
                              end if;
                           end if;

                        when DOUBLE_EQUAL =>

                           if Is_Arith_Exp then
                              if Left.T = NUM and Right.T = NUM then
                                 if Left.I = Right.I then
                                    Push_Result (1);
                                 else
                                    Push_Result (0);
                                 end if;
                              else
                                 if Left.S.all = Right.S.all then
                                    Push_Result (1);
                                 else
                                    Push_Result (0);
                                 end if;
                              end if;
                           else
                              raise Expr_Error;
                           end if;

                        when GT =>
                           if Left.T = NUM and Right.T = NUM then
                              if Left.I > Right.I then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           else
                              if Left.S.all > Right.S.all then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           end if;

                        when GE =>
                           if Left.T = NUM and Right.T = NUM then
                              if Left.I >= Right.I then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           else
                              if Left.S.all >= Right.S.all then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           end if;

                        when LT =>
                           if Left.T = NUM and Right.T = NUM then
                              if Left.I < Right.I then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           else
                              if Left.S.all < Right.S.all then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           end if;

                        when LE =>
                           if Left.T = NUM and Right.T = NUM then
                              if Left.I <= Right.I then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           else
                              if Left.S.all <= Right.S.all then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           end if;

                        when DIFF =>
                           if Left.T = NUM and Right.T = NUM then
                              if Left.I /= Right.I then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           else
                              if Left.S.all /= Right.S.all then
                                 Push_Result (1);
                              else
                                 Push_Result (0);
                              end if;
                           end if;

                        when COLON =>

                           if Is_Arith_Exp then
                              raise Expr_Error;
                           end if;

                           declare
                              Pattern      : String (1 .. Right.S'Length * 2);
                              Pattern_Last : Integer := 0;
                              Level        : Integer := 0;
                              Is_Escape    : Boolean := False;
                           begin
                              for J in Right.S'Range loop
                                 if Is_Escape then
                                    case Right.S (J) is
                                       when '+' | '?' | '|' | ')' | '(' =>
                                          Pattern_Last := Pattern_Last + 1;
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                          if Right.S (J) = '(' then
                                             Level := Level + 1;
                                          end if;
                                          Is_Escape := False;
                                       when '\' =>
                                          Pattern_Last := Pattern_Last + 1;
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                       when others =>
                                          Pattern_Last := Pattern_Last + 2;
                                          Pattern (Pattern_Last - 1) := '\';
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                          Is_Escape := False;
                                    end case;
                                 else
                                    case Right.S (J) is
                                       when '\' => Is_Escape := True;
                                       when '+' =>
                                          Pattern_Last := Pattern_Last + 2;
                                          Pattern (Pattern_Last - 1) := '\';
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                       when '?' =>
                                          Pattern_Last := Pattern_Last + 2;
                                          Pattern (Pattern_Last - 1) := '\';
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                       when '|' =>
                                          Pattern_Last := Pattern_Last + 2;
                                          Pattern (Pattern_Last - 1) := '\';
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                       when others =>
                                          Pattern_Last := Pattern_Last + 1;
                                          Pattern (Pattern_Last) :=
                                            Right.S (J);
                                    end case;
                                 end if;
                              end loop;

                              if Is_Escape then
                                 Pattern_Last := Pattern_Last + 1;
                                 Pattern (Pattern_Last) := '\';
                              end if;

                              declare
                                 M : Match_Array (0 .. Level);
                              begin
                                 Match
                                   ('^' &
                                      Pattern (Pattern'First .. Pattern_Last),
                                    Left.S.all, M);
                                 if M (0) /= No_Match then
                                    if Level = 0 then
                                       Push_Result (1);
                                    else
                                       if M (1) /= No_Match then
                                          Push_Result
                                            (Left.S
                                               (M (1).First .. M (1).Last));
                                       else
                                          Push_Result ("");
                                       end if;
                                    end if;
                                 else
                                    if Level > 0 then
                                       Push_Result ("");
                                    else
                                       Push_Result (0);
                                    end if;
                                 end if;
                              end;
                           end;

                        when others => raise Expr_Error;
                     end case;

                  else
                     raise Expr_Error;
                  end if;

               end loop;

               if CT.T = RIGHT_PAR then
                  --  right parenthesis does not generate any pending operation
                  --  it just triggers the evaluation of the all parenthesis
                  --  block because RIGHT_PAR is the operator with the lowest
                  --  priority
                  Opened_Par := Opened_Par - 1;
               elsif CT.T = LEFT_PAR then
                  --  left parenthesis has a priority superior to all operators
                  --  this means that when CT is LEFT_PAR no evaluation occurs
                  --  we push on the op stack a RIGHT_PAR because it has the
                  --  lowest priority. This mean that when we are between
                  --  parenthesis, this op will be poped only when a RIGHT_PAR
                  --  is encountered.
                  Opened_Par := Opened_Par + 1;
                  Op_Stack_Top := Op_Stack_Top + 1;
                  Op_Stack (Op_Stack_Top) := (RIGHT_PAR, 0, null, null);
               else
                  Op_Stack_Top := Op_Stack_Top + 1;
                  Op_Stack (Op_Stack_Top) := CT;
               end if;
         end case;

      end loop;

      if Opened_Par /= 0 then
         Error (S,
                "')' expected");
         raise Expr_Error;
      else
         return Arg_Stack (1).S.all;
      end if;

   exception
      when Expr_Error =>
         raise Expr_Error;
      when others => return "";

   end Eval_Expr;

end Sh.Subst.Arith;
