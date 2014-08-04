------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Expr                          --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.Regpat;                  use GNAT.Regpat;

with Posix_Shell.String_Utils;     use Posix_Shell.String_Utils;
with Posix_Shell.Traces;           use Posix_Shell.Traces;
with Posix_Shell.Utils;            use Posix_Shell.Utils;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Expr is

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
      EQUAL,         -- '='        |
      GT,            -- '>'        |
      GE,            -- '>='       |
      LT,            -- '<'        |
      LE,            -- '<='       |
      DIFF,          -- '!='       |
      PLUS,          -- '+'        |
      MINUS,         -- '-'        |
      STAR,          -- '*'        |
      DIV,           -- '/'        |
      PERCENT,       -- '%'        |
      COLON,         -- ':'        |
      LEFT_PAR,      -- '('        V highest priority operator
      NUM,
      STRG);          -- numbers

   type Expr_Token is record
      T : Expr_Token_Type;
      I : Integer;
      S : String_Access;
   end record;
   --  expr tokens can be eitheir an operator or a number. When T field is NUM
   --  then I field contains the number value.

   Null_Expr_Token : constant Expr_Token := (EOC, 0, null);

   Expr_Error : exception;

   -------------------
   --  Expr_Builtin --
   -------------------

   function Expr_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
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

      CT : Expr_Token;
      --  Current token

      function Next return Expr_Token;
      procedure Push_Result (I : Integer);
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
                  return (T, 0, null);
               elsif First_Choice /= EOC then
                  return (First_Choice, 0, null);
               else
                  return (STRG, 0, new String'(Str));
               end if;
            elsif Str_Length = 2 and then
              Second_Char = Str (Str'First + 1)
            then
               return (T, 0, null);
            else
               return (STRG, 1, new String'(Str));
            end if;
         end Return_Op_Token;

      begin
         Index := Index + 1;

         if Str_Length = 0 then
            return (STRG, 0, new String'(""));
         end if;

         case Str (Str'First) is
            when ')' => return Return_Op_Token (RIGHT_PAR);
            when '(' => return Return_Op_Token (LEFT_PAR);
            when '|' => return Return_Op_Token (PIPE);
            when '&' => return Return_Op_Token (CAND);
            when '=' => return Return_Op_Token (EQUAL);
            when '>' => return Return_Op_Token (GE, '=', GT);
            when '<' => return Return_Op_Token (LE, '=', LT);
            when '!' => return Return_Op_Token (DIFF, '=');
            when '+' => return Return_Op_Token (PLUS);
            when '*' => return Return_Op_Token (STAR);
            when '/' => return Return_Op_Token (DIV);
            when '%' => return Return_Op_Token (PERCENT);
            when ':' => return Return_Op_Token (COLON);
            when '0' .. '9' | '-' =>
               if Str_Length = 1 and Str (Str'First) = '-' then
                  return (MINUS, 0, null);
               end if;

               To_Integer (Str, N, Is_Num);

               if Is_Num then
                  return (NUM, N, new String'(Str));
               else
                  return (STRG, 1, new String'(Str));
               end if;

            when others => return (STRG, 1, new String'(Str));
         end case;

      end Next;

      procedure Push_Result (I : Integer) is
      begin
         Free (Arg_Stack (Arg_Stack_Top).S);
         Free (Arg_Stack (Arg_Stack_Top - 1).S);
         --  remove the arguments of the eval
         Arg_Stack_Top := Arg_Stack_Top - 1;

         --  push the result
         Arg_Stack (Arg_Stack_Top) := (NUM, I, new String'(To_String (I)));
      end Push_Result;

      procedure Push_Result (S : String) is
      begin

         Arg_Stack_Top := Arg_Stack_Top - 1;
         if S = "" then
            Arg_Stack (Arg_Stack_Top) := (STRG, 0, new String'(S));
         else
            Arg_Stack (Arg_Stack_Top) := (STRG, 0, new String'(S));
         end if;
      end Push_Result;

      procedure Push_Result (T : Expr_Token) is
      begin
         if T.S /= Arg_Stack (Arg_Stack_Top).S then
            Free (Arg_Stack (Arg_Stack_Top).S);
         end if;

         if T.S /= Arg_Stack (Arg_Stack_Top - 1).S then
            Free (Arg_Stack (Arg_Stack_Top - 1).S);
         end if;

         Arg_Stack_Top := Arg_Stack_Top - 1;

         --  push the result
         Arg_Stack (Arg_Stack_Top) := T;
      end Push_Result;

      Op : Expr_Token_Type;
      Left, Right : Expr_Token;
   begin
      pragma Debug (Log ("expr", "start"));
      loop
         exit when Index > Args'Last + 1;

         if Index = Args'Last + 1 then
            --  We have reached the end of the expression passed as argument of
            --  expr so force evaluation of the remaining op in the stack
            CT := (EOC, 0, null); Index := Index + 1;
         else
            CT := Next;
         end if;

         case CT.T is
            when NUM | STRG =>
               Arg_Stack_Top := Arg_Stack_Top + 1;
               Arg_Stack (Arg_Stack_Top) := CT;
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
                           declare
                              Pattern : String (1 .. Right.S'Length * 2);
                              Pattern_Last : Integer := 0;
                              Level : Integer := 0;
                              Is_Escape : Boolean := False;
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
                                            (Left.S.all
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
                  null;
               elsif CT.T = LEFT_PAR then
                  --  left parenthesis has a priority superior to all operators
                  --  this means that when CT is LEFT_PAR no evaluation occurs
                  --  we push on the op stack a RIGHT_PAR because it has the
                  --  lowest priority. This mean that when we are between
                  --  parenthesis, this op will be poped only when a RIGHT_PAR
                  --  is encountered.
                  Op_Stack_Top := Op_Stack_Top + 1;
                  Op_Stack (Op_Stack_Top) := (RIGHT_PAR, 0, null);
               else
                  Op_Stack_Top := Op_Stack_Top + 1;
                  Op_Stack (Op_Stack_Top) := CT;
               end if;
         end case;

      end loop;

      Put (S.all, 1, Arg_Stack (1).S.all & ASCII.LF);
      if Arg_Stack (1).S.all = "0" or else Arg_Stack (1).S.all = "" then
         return 1;
      else
         return 0;
      end if;
   exception
      when Expr_Error =>
         Put (S.all, 2, "invalid expression" & ASCII.LF);
         return 2;
   end Expr_Builtin;

end Posix_Shell.Builtins.Expr;
