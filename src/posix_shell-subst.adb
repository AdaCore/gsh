------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                              Posix_Shell.Subst                           --
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

with Posix_Shell.Variables.Output;     use Posix_Shell.Variables.Output;
with Posix_Shell.Parser;     use Posix_Shell.Parser;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
with Posix_Shell.String_Utils;      use Posix_Shell.String_Utils;
with Ada.Strings.Unbounded;
with Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Dyn_String_Lists;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;
with Posix_Shell.Traces; use Posix_Shell.Traces;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Posix_Shell.Buffers; use Posix_Shell.Buffers;
with Posix_Shell.Exec; use Posix_Shell.Exec;

package body Posix_Shell.Subst is

   function Split_String
     (SS        : Shell_State_Access;
      S         : Annotated_String;
      Max_Split : Integer := -1)
      return Dyn_String_Lists.Dyn_String_List;

   function Eval_String_Aux
     (SS              : Shell_State_Access;
      S               : String;
      Characters_Read : out Integer;
      Case_Pattern    : Boolean := False;
      IOHere          : Boolean := False;
      Is_Splitable    : Boolean := True;
      Is_Param_Subst  : Boolean := False)
      return Annotated_String;

   function Eval_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return Dyn_String_Lists.Dyn_String_List;

   function Simple_Filename_Expansion
     (SS        : Shell_State_Access;
      Dir       : String;
      Pattern   : String;
      Only_Dirs : Boolean := False)
      return String_List;
   --  Return the list of files/directories in Dir that match the glob pattern
   --  Pattern. If Only_Dirs is set to True, only directories are taken into
   --  accounts. If the pattern is not valid or if no files are matching it
   --  then it resturns an empty list.

   Empty_Set : constant String_List := (1 => new String'(""));

   function Filename_Expansion
     (SS  : Shell_State_Access;
      D   : String;
      Set : String_List := Empty_Set)
      return String_List;

   function Strip (S : String) return String;
   --  Strip any CR in the string and also all trailing LF.

   function Eval_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return Dyn_String_Lists.Dyn_String_List
   is
      use Ada.Strings.Unbounded;
      use Dyn_String_Lists;

      Characters_Read : Integer := 0;
      Result          : constant Annotated_String := Eval_String_Aux
        (SS, S, Characters_Read);
   begin
      return Split_String (SS, Result, Max_Split);
   end Eval_String;

   -----------------
   -- Split_String --
   -----------------

   function Split_String
     (SS        : Shell_State_Access;
      S         : Annotated_String;
      Max_Split : Integer := -1)
      return Dyn_String_Lists.Dyn_String_List
   is
      use Ada.Strings.Unbounded;
      use Dyn_String_Lists;

      Result_List : Dyn_String_List;

      type State is
        (EMPTY_FIELD,
         FORCED_EMPTY_FIELD,
         NULL_WORD_FIELD,
         QUOTED_WORD_FIELD,
         WORD_FIELD);

      Current_State     : State := EMPTY_FIELD;
      Buffer            : Unbounded_String := To_Unbounded_String ("");
      Split_Count       : Integer := 0;
      --  Number of fields encountered so far
      Unsplitable_Level : Integer := 0;

      procedure Delimit_Word;
      --  A new word has been found. Append it to the Result_List buffer

      ------------------
      -- Delimit_Word --
      ------------------

      procedure Delimit_Word is
      begin
         case Current_State is
            when EMPTY_FIELD =>
               null;
            when FORCED_EMPTY_FIELD =>
               null;
            when NULL_WORD_FIELD =>
               --  We have a null word so create an empty field
               Append (Result_List, new String'(""));
            when QUOTED_WORD_FIELD =>
               Append (Result_List, new String'(To_String (Buffer)));
            when WORD_FIELD =>
               if Is_File_Expansion_Enabled (SS.all) then
                  Append (Result_List,
                          Filename_Expansion (SS, To_String (Buffer)));
               else
                  Append (Result_List,
                          new String'(To_String (Buffer)));
               end if;

         end case;

         if Current_State /= EMPTY_FIELD then
            Current_State := EMPTY_FIELD;
            Split_Count := Split_Count + 1;
            Buffer := To_Unbounded_String ("");
         end if;

         if Unsplitable_Level > 0 then
            Current_State := NULL_WORD_FIELD;
         end if;

      end Delimit_Word;

      --  Null_Result : constant String_List (1 .. 0) := (others => null);

      IFS_Value : constant String := Get_Var_Value (SS.all, "IFS");
      --  Get current value of IFS
      IFS_Is_Set : constant Boolean := Is_Var_Set (SS.all, "IFS");
      --  Field splitting behaves differently if IFS="" or IFS is unset

      function In_IFS (C : Character) return Boolean;
      --  Return true if the current character should be considered as field
      --  separator

      ------------
      -- In_IFS --
      ------------

      function In_IFS (C : Character) return Boolean is
      begin
         if IFS_Is_Set then
            for Index in IFS_Value'Range loop
               if IFS_Value (Index) = C then
                  return True;
               end if;
            end loop;
         else
            --  If IFS is unset then behaves as IFS is ' ', tab or lf
            if C = ' ' or else C = ASCII.LF or else C = ASCII.HT then
               return True;
            end if;
         end if;

         return False;
      end In_IFS;

   begin

      --  Perform field splitting
      for I in 1 .. Length (S) loop

         declare
            El : constant Str_Element := Get_Element (S, I);
         begin
            case El.Kind is
               when E_CTRL =>
                  case El.Ctrl is
                     when FIELD_SEP =>
                        --  Force a field splitting even there is no character
                        --  from IFS. This is used to handle special expansion
                        --- of $@ and $*
                        Delimit_Word;
                     when NULL_STRING =>
                        --  We have a null string character. If we are inside
                        --  a word then ignore it, otherwise the current field
                        --  is currently a null word that should not be
                        --  ignored.
                        if Current_State = EMPTY_FIELD then
                           Current_State := NULL_WORD_FIELD;
                        end if;
                     when QUOTED_NULL_STRING =>
                        --  In that case no empty word should be generated if
                        --  the word contains only QUOTED_NULL_STRING and
                        --  NULL_STRING.
                        if Current_State = EMPTY_FIELD or else
                          Current_State = NULL_WORD_FIELD
                        then
                           Current_State := FORCED_EMPTY_FIELD;
                        end if;
                     when UNSPLITABLE_BEGIN =>
                        --  The character is marked as unsplitable so don't
                        --  check for field splitting
                        --  Current_State := QUOTED_WORD_FIELD;
                        Unsplitable_Level := Unsplitable_Level + 1;
                        if Current_State = EMPTY_FIELD then
                           Current_State := NULL_WORD_FIELD;
                        end if;
                     when UNSPLITABLE_END =>
                        Unsplitable_Level := Unsplitable_Level - 1;
                  end case;
               when E_CHAR =>
                  if In_IFS (El.Char) and then Unsplitable_Level = 0 and then
                    (Max_Split = -1 or else Split_Count < Max_Split)
                  then
                     Delimit_Word;
                  else
                     if Current_State /= QUOTED_WORD_FIELD then
                        Current_State := WORD_FIELD;
                     end if;

                     if Unsplitable_Level > 0 then
                        Current_State := QUOTED_WORD_FIELD;
                     end if;
                     Buffer := Buffer & El.Char;
                  end if;
               when E_NULL =>
                  raise Program_Error;
            end case;
         end;
      end loop;

      --  Get last characters from the buffer and delimit the last field
      Delimit_Word;

      --  if Last (Result_List) = 0 then
      --    return Null_Result;
      --  else
      return Result_List;

      --  end if;
   end Split_String;

   function Split_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return String_List
   is
      use Dyn_String_Lists;
      AS : Annotated_String;
   begin
      Append (AS, S);
      return Content (Split_String (SS, AS, Max_Split));
   end Split_String;

   function Eval_String
     (SS        : Shell_State_Access;
      S         : String;
      Max_Split : Integer := -1)
      return String_List
   is
      use Dyn_String_Lists;
   begin
      return Content (Eval_String (SS, S, Max_Split));
   end Eval_String;

   ---------------------
   -- Eval_String_Aux --
   ---------------------

   function Eval_String_Aux
     (SS              : Shell_State_Access;
      S               : String;
      Characters_Read : out Integer;
      Case_Pattern    : Boolean := False;
      IOHere          : Boolean := False;
      Is_Splitable    : Boolean := True;
      Is_Param_Subst  : Boolean := False)
      return Annotated_String
   is

      Buffer : Annotated_String;

      Index : Integer := S'First;

      procedure Eval_Single_Quote;
      --  Eval a single quoted string (i.e of form 'xxxxx')

      procedure Eval_Param_Subst (Is_Splitable : Boolean);
      --  Eval a parameter substitution construction

      procedure Eval_Backquoted_Command_Subst (In_Double_Quote : Boolean);
      --  Eval a command substitution starting with a backquote

      procedure Eval_Command_Subst;
      --  Eval a command substitution construction

      procedure Eval_Double_Quote;
      --  Eval a double quoted string

      procedure Eval_Escape_Sequence (In_Double_Quote : Boolean);
      --  Eval an escape sequence

      function Read_Parameter
        (Is_Brace_Expansion : Boolean)
         return String;

      procedure Apply_Substitution_Op
        (Parameter    : String;
         Operator     : String;
         Is_Splitable : Boolean);

      -----------------------------------
      -- Eval_Backquoted_Command_Subst --
      -----------------------------------

      procedure Eval_Backquoted_Command_Subst (In_Double_Quote : Boolean) is
         Start_Index  : Integer;
         Command      : String (1 .. S'Last - Index);
         Command_Last : Integer := 0;

      begin
         Index := Index + 1; -- skip backquote
         Start_Index := Index;

         loop
            case S (Index) is
               when '\' =>
                  Index := Index + 1;
                  case S (Index) is
                     when '$' | '`' | '\' =>
                        Command_Last := Command_Last + 1;
                        Command (Command_Last) := S (Index);
                     when '"' =>
                        if not In_Double_Quote then
                           Command_Last := Command_Last + 1;
                           Command (Command_Last) := '\';
                        end if;
                        Command_Last := Command_Last + 1;
                        Command (Command_Last) := '"';
                     when others =>
                        Command_Last := Command_Last + 1;
                        Command (Command_Last) := '\';
                        Index := Index - 1;
                  end case;
               when '`' =>
                  exit;
               when others =>
                  Command_Last := Command_Last + 1;
                  Command (Command_Last) := S (Index);
            end case;
            Index := Index + 1;
         end loop;

         if Start_Index <= Index - 1 then
            declare
               T : Shell_Tree;
               Str : constant String := Command (1 .. Command_Last);
            begin
               pragma Debug (Log ("command subst", Str));
               T := Parse_String (Str);
               Append (Buffer, Strip (Eval (SS, T)));
               Free_Node (T);
            end;
         end if;
      end Eval_Backquoted_Command_Subst;

      ------------------------
      -- Eval_Command_Subst --
      ------------------------

      procedure Eval_Command_Subst is
      begin
         Index := Index + 1;
         --  skip the initial parenthesis. dollar has already been skipped

         declare
            Str : constant String := S (Index .. S'Last);
            Buf : Token_Buffer := New_Buffer (Str);
            T   : Shell_Tree;
         begin
            T := Parse_Buffer (Buf, Until_Token => T_RPAR);
            Append (Buffer, Strip (Eval (SS, T)));
            Free_Node (T);
            Index := Index + Offset (Buf.Previous_Token_Pos) - 1;
            pragma Debug (Log ("command subst",
                               "'" & S (Index .. S'Last) & "'"));
         end;

      end Eval_Command_Subst;

      -----------------------
      -- Eval_Double_Quote --
      -----------------------

      procedure Eval_Double_Quote is
         Saved_Buffer_Length : constant Integer := Length (Buffer);
         CC                  : Character;
      begin
         Index := Index + 1; -- skip the first double quote
         Append (Buffer, UNSPLITABLE_BEGIN);
         loop
            CC := S (Index);

            case CC is
               when '`' =>
                  Eval_Backquoted_Command_Subst (True);
               when '$' =>
                  Eval_Param_Subst (Is_Splitable => False);
               when '\' =>
                  Eval_Escape_Sequence (In_Double_Quote => True);
               when '"' =>
                  if Length (Buffer) = Saved_Buffer_Length then
                     Append (Buffer, NULL_STRING);
                  end if;
                  exit;
               when others =>
                  Append (Buffer, CC);
            end case;

            Index := Index + 1;
         end loop;
         Append (Buffer, UNSPLITABLE_END);

      end Eval_Double_Quote;

       --------------------
       -- Read_Parameter --
       --------------------

      function Read_Parameter
        (Is_Brace_Expansion : Boolean)
         return String
      is
         Param_First : constant Integer := Index;

      begin
            if S (Index) = '@'  --  Special parameter
              or else S (Index) = '*'  --  Special parameter
              or else S (Index) = '#'  --  Special parameter
              or else S (Index) = '?'  --  Special parameter
              or else S (Index) = '-'  --  Special parameter
              or else S (Index) = '$'  --  Special parameter
              or else S (Index) = '!'  --  Special parameter
            then
               --  Is this special parameter? In such case,
               --  the token ends at the first character.
               return "" & S (Index);

            elsif not Is_Brace_Expansion and then S (Index) in '0' .. '9' then
               return "" & S (Index);

            else
               --  This is neither a brace parameter expansion, neither a
               --  special parameter. This remaining case is for $PARAMETER
               --  where parameter is a valid variable name.

               loop

                  case S (Index) is
                     when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
                        null;
                     when others =>
                        Index := Index - 1;
                        exit;
                  end case;
                  exit when Index >= S'Last;

                  Index := Index + 1;
               end loop;
               pragma Debug (Log ("read_parameter", S (Param_First .. Index)));
               return S (Param_First .. Index);
            end if;
      end Read_Parameter;

      procedure Apply_Substitution_Op
        (Parameter    : String;
         Operator     : String;
         Is_Splitable : Boolean)
      is
         Param_Value   : constant Annotated_String :=
           Get_Var_Value (SS.all, Parameter, Is_Splitable);
         Is_Null       : constant Boolean := Str (Param_Value)'Length = 0;
         Is_Set        : Boolean := Is_Var_Set (SS.all, Parameter);
         Word          : Annotated_String;
         Read          : Integer := 0;

      begin
         if Operator (Operator'First) = ':' and then Is_Null then
            Is_Set := False;
         end if;

         --  First eval the word that follow the operator
         Append (Word,
                 Eval_String_Aux (SS,
                   S (Index .. S'Last),
                   Read,
                   Is_Splitable => Is_Splitable,
                   Is_Param_Subst => True));
         Index := Index + Read - 1;

         --  Then decide what to do depending on the parameter value and the
         --  operator
         if Operator (Operator'Last) = '-' then
            if not Is_Set then
               Append (Buffer, Word);
            elsif not Is_Null then
               Append (Buffer, Param_Value);
            end if;
         elsif Operator (Operator'Last) = '=' then
            if not Is_Set then
               Set_Var_Value (SS.all,
                              Name => Parameter,
                              Value => Str (Word));
               Append (Buffer, Word);
            else
               Append (Buffer, Param_Value);
            end if;
         elsif Operator (Operator'Last) = '+' then
            if Is_Set then
               Append (Buffer, Word);
            end if;
         elsif Operator (Operator'Last) = '?' then
            if not Is_Set then
               declare
                  Message : constant String := Str (Word);
               begin
                  if Message'Length > 0 then
                     Error (SS.all, Parameter & ": " & Message);
                  else
                     Error (SS.all, Parameter & ": parameter null or not set");
                  end if;
               end;
               Shell_Exit (SS.all, 1);
            end if;
         end if;
      end Apply_Substitution_Op;

      ----------------------
      -- Eval_Param_Subst --
      ----------------------

      procedure Eval_Param_Subst (Is_Splitable : Boolean) is
         Param_First           : Integer;
         Is_Brace_Expansion    : Boolean := False;
         --  By default, we do not need to do parameter expansion.
         --  We do the expansion when we have "${expression}".
         CC                    : Character;

      begin
         --  Skip the initial '$'...
         Index := Index + 1;

         --  Check if the kind of expansion (with braces or not). Using braces
         --  allows the use of more complex patterns for the substitution.
         if S (Index) = '{' then
            Is_Brace_Expansion := True;
            Index := Index + 1;
         elsif S (Index) = '(' then
            --  This is a command substitution not a parameter one.
            Eval_Command_Subst;
            return;
         end if;

         --  All parameters expansions do start with a parameter.
         Param_First := Index;
         CC := S (Param_First);

         pragma Debug (Log ("paremeter_subst", Is_Brace_Expansion'Img));

         if Is_Brace_Expansion then
            --  This is most complex type of exansion
            if CC = '#' and then
              (S (Index + 1) = '_' or else
               S (Index + 1) in 'a' .. 'z' or else
               S (Index + 1) in 'A' .. 'Z')
            then
               --  We should compute the length of the upcoming parameter
               Index := Index + 1;
               declare
                  Parameter : constant String :=
                    Read_Parameter (Is_Brace_Expansion);
                  Length    : constant String :=
                    To_String
                      (Str
                         (Get_Var_Value
                            (SS.all, Parameter, Is_Splitable))'Length);
               begin
                  Index := Index + 1;
                  if S (Index) /= '}' then
                     Error (SS.all, "bad substitution");
                     raise Variable_Name_Error;
                  end if;
                  --  The previous test ensure that there is a parameter.
                  Append (Buffer, Length);
               end;
            else
               --  In all other cases we should first find a parameter
               declare
                  Parameter : constant String :=
                    Read_Parameter (Is_Brace_Expansion);
               begin
                  Index := Index + 1;

                  case S (Index) is
                     when '}' =>
                        Append
                          (Buffer,
                           Get_Var_Value (SS.all, Parameter, Is_Splitable));
                        return;
                     when '-' | '=' | '?' | '+' =>
                        Index := Index + 1;
                        Apply_Substitution_Op (Parameter,
                                               S (Index - 1 .. Index - 1),
                                               Is_Splitable);
                     when ':' =>
                        Index := Index + 1;
                        case S (Index) is
                           when '-' | '=' | '?' | '+' =>
                              Index := Index + 1;
                              Apply_Substitution_Op
                                (Parameter,
                                 S (Index - 2 .. Index - 1),
                                 Is_Splitable);
                           when others =>
                              Error (SS.all, "bad substitution");
                              raise Variable_Name_Error;
                        end case;
                     when others =>
                        Error (SS.all, "bad substitution");
                        raise Variable_Name_Error;
                  end case;
               end;
            end if;
         else
            declare
               Parameter : constant String := Read_Parameter
                 (Is_Brace_Expansion);
            begin
               if Parameter'Length = 0 then
                  Append (Buffer, '$');
               else
                  Append
                    (Buffer,
                     Get_Var_Value (SS.all, Parameter, Is_Splitable));
               end if;
            end;
         end if;
      exception
         when Variable_Name_Error =>
            Error (SS.all, "bad substitution");
            raise;
      end Eval_Param_Subst;

      -----------------------
      -- Eval_Single_Quote --
      -----------------------

      procedure Eval_Single_Quote is
         Saved_Buffer_Length : constant Integer := Length (Buffer);
         CC                  : Character;
      begin
         Index := Index + 1; --  skip the opening single quote
         Append (Buffer, UNSPLITABLE_BEGIN);
         loop
            CC := S (Index);
            case CC is
               when ''' =>
                  if Length (Buffer) = Saved_Buffer_Length then
                     Append (Buffer, NULL_STRING);
                  end if;
                  exit;

               when others =>
                  Append (Buffer, CC);
            end case;
            Index := Index + 1;
         end loop;
         Append (Buffer, UNSPLITABLE_END);
      end Eval_Single_Quote;

      --------------------------
      -- Eval_Escape_Sequence --
      --------------------------

      procedure Eval_Escape_Sequence (In_Double_Quote : Boolean) is
         CC : Character;
      begin
         Index := Index + 1;
         CC := S (Index);

         if CC /= ASCII.LF then
            --  This also refer implicitely to Section 2.3 Rule 11

            if not In_Double_Quote or else
              (CC = '"' and not IOHere) or else
              CC = '`' or else
              CC = '$' or else
              CC = '\'
            then
               if not Case_Pattern then
                  Append (Buffer, UNSPLITABLE_BEGIN);
                  Append (Buffer, CC);
                  Append (Buffer, UNSPLITABLE_END);
               else
                  case CC is
                     when '[' | ']' | '*' | '?' =>
                        Append (Buffer, '\');
                        Append (Buffer, CC);
                     when others =>
                        Append (Buffer, CC);
                  end case;
               end if;
            else
               Append (Buffer, '\');
               Index := Index - 1;
            end if;
         end if;
      end Eval_Escape_Sequence;

      CC : Character;
   begin
      pragma Debug (Log ("eval_string_aux_begin", S));
      while Index <= S'Last loop
         CC := S (Index);

         case CC is
            when ''' =>
               if IOHere then
                  Append (Buffer, CC);
               else
                  Eval_Single_Quote;
               end if;
            when '"' =>
               if IOHere then
                  Append (Buffer, CC);
               else
                  Eval_Double_Quote;
               end if;

            when '`' =>
               Eval_Backquoted_Command_Subst (False);

            when '$' =>
               Eval_Param_Subst (Is_Splitable);

            when '\' =>
               Eval_Escape_Sequence (IOHere);
            when others =>
               if Is_Param_Subst and then CC = '}' then
                  Index := Index + 1;
                  exit;
               end if;
               Append (Buffer, CC);

         end case;
         Index := Index + 1;
      end loop;

      pragma Debug (Log ("eval_string_aux", Image (Buffer)));
      Characters_Read := Index - S'First;
      return Buffer;
   end Eval_String_Aux;

   ----------------------
   -- Eval_String_List --
   ----------------------

   function Eval_String_List
     (SS : Shell_State_Access;
      T  : Shell_Tree;
      S  : Token_List)
      return String_List
   is
      use Dyn_String_Lists;
      Result : Dyn_String_List;
      Pool   : constant List_Pool := Token_List_Pool (T);
      Cursor : Token_List := S;
   begin
      while Cursor /= Null_List loop
         pragma Debug (Log ("eval_string_list", "cursor: " & Cursor'Img));
         declare
            Elem_Eval : constant Dyn_String_List :=
              Eval_String (SS, Get_Token_String (Get_Element (Pool, Cursor)));
         begin
            Append (Result, Elem_Eval);
         end;

         Cursor := Next (Pool, Cursor);
      end loop;

      return Content (Result);
   end Eval_String_List;

   -------------------------
   -- Eval_String_Unsplit --
   -------------------------

   function Eval_String_Unsplit
     (SS                 : Shell_State_Access;
      S                  : String;
      Case_Pattern       : Boolean := False;
      IOHere             : Boolean := False)
      return String
   is
      Characters_Read   : Integer := 0;
      Result            : constant Annotated_String := Eval_String_Aux
        (SS, S, Characters_Read, Case_Pattern, IOHere);
      --  we create a buffer or twice the size as some characters might be
      --  excaped in case construct context
      Result_String     : String (1 .. Length (Result) * 2);
      Result_Index      : Integer := 1;
      Unsplitable_Level : Integer := 0;
   begin
      if Case_Pattern then
         for I in 1 .. Length (Result) loop
            declare
               Current_El : constant Str_Element := Get_Element (Result, I);
            begin
               if Current_El = (E_CTRL, UNSPLITABLE_BEGIN) then
                  Unsplitable_Level := Unsplitable_Level + 1;
               elsif Current_El = (E_CTRL, UNSPLITABLE_END) then
                  Unsplitable_Level := Unsplitable_Level - 1;
               elsif Current_El.Kind = E_CHAR then

                  if Unsplitable_Level > 0 then
                     case Current_El.Char is
                        when '*' | '?' | '[' | ']' =>
                           Result_String (Result_Index) := '\';
                           Result_Index := Result_Index + 1;
                        when others =>
                           null;
                     end case;
                  end if;
                  Result_String (Result_Index) := Current_El.Char;
                  Result_Index := Result_Index + 1;
               end if;
            end;
         end loop;

         if Result_Index = 1 then
            return "";
         else
            return Result_String (Result_String'First .. Result_Index - 1);
         end if;
      else
         return Str (Result);
      end if;
   end Eval_String_Unsplit;

   ------------------------
   -- Filename_Expansion --
   ------------------------

   function Filename_Expansion
     (SS  : Shell_State_Access;
      D   : String;
      Set : String_List := Empty_Set)
      return String_List
   is
      use Dyn_String_Lists;
      Got_Open_Par  : Boolean := False;
      Need_Eval     : Boolean := False;
      Last_Slash    : Integer := D'First - 1;
      Pattern_End   : Integer := D'Last;
      Pattern_Begin : Integer;
      Only_Dirs     : Boolean := False;
      CWD           : String_Access := null;
      Entry_Buffer  : Dyn_String_List;

      function Result (R : String_List) return String_List;
      pragma Inline (Result);

      function Result (R : String_List) return String_List is
      begin
         Free (CWD);
         return R;
      end Result;

   begin

      --  Detect pattern presence
      for I in D'Range loop
         case D (I) is
            when '[' => Got_Open_Par := True;
            when ']' =>
               if Got_Open_Par then
                  Need_Eval := True;
                  Pattern_Begin := Last_Slash + 1;
               end if;
            when '*' | '?' =>
               Need_Eval := True;
               Pattern_Begin := Last_Slash + 1;
            when '/' =>
               if not Need_Eval then
                  Last_Slash := I;
               else
                  Pattern_End := I - 1;
                  exit;
               end if;
            when others => null;
         end case;
      end loop;

      --  No pattern has been found
      if not Need_Eval then
         if Set = Empty_Set then
            --  If at toplevel then we can return the "pattern"
            return (1 => new String'(D));
         else
            declare
               Buffer : String_List (1 .. Set'Length);
               Buffer_Last : Natural := 0;
            begin
               --  otherwise check entries existence
               for I in Set'Range loop
                  declare
                     Tmp : constant String := Resolve_Path
                       (SS.all, Set (I).all & D);
                  begin
                     if GNAT.OS_Lib.Is_Regular_File (Tmp) or else
                       GNAT.OS_Lib.Is_Directory (Tmp)
                     then
                        Buffer_Last := Buffer_Last + 1;
                        Buffer (Buffer_Last) := new String'(Set (I).all & D);
                     end if;
                  end;
               end loop;
               return Buffer (1 .. Buffer_Last);
            end;
         end if;
      end if;

      --  Set the current working directory
      if Last_Slash >= D'First then
         CWD := new String'(D (D'First .. Last_Slash));
      else
         CWD := new String'("");
      end if;

      --  Are we interested only in directory entries ?
      if Pattern_End < D'Last then
         Only_Dirs := True;
      end if;

      --  Find the entries
      for I in Set'Range loop
         if CWD.all = "" or else
              GNAT.OS_Lib.Is_Directory
                (Resolve_Path (SS.all, Set (I).all & CWD.all))
         then
            declare
               L : constant String_List := Simple_Filename_Expansion
                 (SS, Set (I).all & CWD.all,
                  D (Pattern_Begin .. Pattern_End),
                  Only_Dirs);
            begin
               Append (Entry_Buffer, L);
               --  Set_Last (Entry_Buffer, Last (Entry_Buffer) + L'Length);
               --  Entry_Buffer.Table
               --  (Last (Entry_Buffer) - L'Length + 1 .. Last (Entry_Buffer))
               --   := Table_Type (L);
            end;
         end if;
      end loop;

      if Length (Entry_Buffer) < 1 then
         --  No entry has been found. If at toplevel return the pattern.
         --  Otherwise propagate a null length string list
         if Set = Empty_Set then
            return Result ((1 => new String'(D)));
         else
            return Result (Null_String_List);
         end if;
      else
         --  some entry have been found that match the first encountered
         --  pattern start the recursion if needed
         if Pattern_End < D'Last - 1 then
            declare
               Tmp : constant String_List := Filename_Expansion
                 (SS, D (Pattern_End + 2 .. D'Last),
                  Content (Entry_Buffer));
            begin
               if Set = Empty_Set and Tmp = Null_String_List then
                  return Result ((1 => new String'(D)));
               else
                  return Result (Tmp);
               end if;
            end;
         else
            return Result
              (Content (Entry_Buffer));
         end if;
      end if;
   end Filename_Expansion;

   -------------------------------
   -- Simple_Filename_Expansion --
   -------------------------------

   function Simple_Filename_Expansion
     (SS        : Shell_State_Access;
      Dir       : String;
      Pattern   : String;
      Only_Dirs : Boolean := False)
      return String_List
   is
      use Dyn_String_Lists;
      use Ada.Directories;
      Result  : Dyn_String_List;
      --  Dynamic String that will contains the result of expansion

      S       : Search_Type;
      D_Entry : Directory_Entry_Type;

      Prefix  : String_Access := new String'("");
   begin

      --  Initiate seach. Return a null string_list in case the pattern is not
      --  valid
      begin
         if Dir = "" then
            Prefix := new String'("");
            Start_Search (S, Resolve_Path (SS.all, "."), Pattern);
         else
            Prefix := new String'(Format_Pathname (Dir, UNIX));
            Start_Search (S, Resolve_Path (SS.all, Dir), Pattern);
         end if;
      exception
         when Name_Error =>
            Free (Prefix);
            return Null_String_List;
      end;

      while More_Entries (S) loop
         Get_Next_Entry (S, D_Entry);
         declare
            Item : constant String := Simple_Name (D_Entry);
         begin
            if Item (Item'First) /= '.' and then
              (not Only_Dirs or else Kind (D_Entry) = Directory)
            then
               if Only_Dirs then
                  Append (Result, new String'(Prefix.all & Item & "/"));
               else
                  Append (Result, new String'(Prefix.all & Item));
               end if;
            end if;
         end;
      end loop;

      Free (Prefix);

      return Content (Result);
   end Simple_Filename_Expansion;

   -----------
   -- Strip --
   -----------

   function Strip (S : String) return String is
   begin
      --  First handle case in which string length is 0
      if S'Length = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. S'Length);
         --  Buffer that contains the result

         Result_First : Integer := -1;
         --  Final returned value will be Result (Result_First .. Result'Last)
         --  as scan is done in backward direction.

         Cursor : Integer := S'Last;
         --  Index of the currently scanned character
      begin

         loop
            case S (Cursor) is
               when ASCII.LF =>
                  --  Skip LF character only trailing LF
                  if Result_First /= -1 then
                     Result (Result_First) := ASCII.LF;
                     Result_First := Result_First - 1;
                  end if;
               when ASCII.CR =>
                  --  Skip all CR characters
                  null;
               when others =>
                  if Result_First = -1 then
                     Result_First := Result'Last;
                  end if;
                  Result (Result_First) := S (Cursor);
                  Result_First := Result_First - 1;
            end case;
            Cursor := Cursor - 1;
            exit when Cursor < S'First;
         end loop;

         --  Return resulting string if any
         if Result_First = -1 then
            return "";
         else
            return Result (Result_First + 1 .. Result'Last);
         end if;

      end;
   end Strip;

end Posix_Shell.Subst;
