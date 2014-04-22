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

with Posix_Shell.Exec;       use Posix_Shell.Exec;
with Posix_Shell.Variables.Output;     use Posix_Shell.Variables.Output;
with Posix_Shell.Parser;     use Posix_Shell.Parser;
with Posix_Shell.Tree;       use Posix_Shell.Tree;
with Posix_Shell.Utils;      use Posix_Shell.Utils;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
with Posix_Shell.GNULib;     use Posix_Shell.GNULib;

with Ada.Strings.Unbounded;
with Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Dyn_String_Lists;

package body Posix_Shell.Subst is

   function Eval_String_Aux
     (SS           : Shell_State_Access;
      S            : Annotated_String;
      Case_Pattern : Boolean := False;
      Quote_Removal_Only : Boolean := False;
      Context : Annotation := NO_ANNOTATION)
      return Annotated_String;

   function Expanded_Parameter (SS : Shell_State_Access;
                                Param_Name : String;
                                Word : Annotated_String;
                                Modifier : Character;
                                Treat_Null_As_Unset : Boolean;
                                Largest : Boolean;
                                Note : Annotation)
     return Annotated_String;
   --  Perform the expansion of the given parameter and word following
   --  the given Modifier. Normally, the expansion treats a null parameter
   --  as a set parameter.  However, if Treat_Null_As_Unset is true, then
   --  a null parameter will be treated as if it was unset.

   function Eval_Param_Expression (State : Shell_State_Access;
                                   Expr : Annotated_String;
                                   Do_Expansion : Boolean;
                                   Note : Annotation)
     return Annotated_String;
   --  Evaluate the given parameter expression into its actual value.
   --  If Do_Expansion is True, then parameter expansion modifiers
   --  (such as "-", or ":=", ":+", "?", etc) will be taken into account.

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

   ---------------------------
   -- Eval_Param_Expression --
   ---------------------------

   function Eval_Param_Expression (State : Shell_State_Access;
                                   Expr : Annotated_String;
                                   Do_Expansion : Boolean;
                                   Note : Annotation)
     return Annotated_String
   is
      E : constant String := Str (Expr);
      Modifier : Character := ASCII.NUL;
      Parameter_Last : Integer;
      Word_First : Integer;
      Treat_Null_As_Unset : Boolean := False;
      Largest : Boolean := False;
   begin
      --  If no expansion is needed, then the expression is actually
      --  a variable name. Return its value.
      if not Do_Expansion then
         return Get_Var_Value (State.all, E, Note);
      end if;

      --  If the variable name starts with a '#', then parameter
      --  is a variable.  We need to evaluate it and return its length.
      if E (E'First) = '#' and then E'Length > 1 then
         declare
            Param : String renames E (E'First + 1 .. E'Last);
            Value : constant String := Get_Var_Value (State.all, Param);
         begin
            if Is_Valid_Variable_Name (Param) then
               return To_Annotated_String (To_String (Value'Length), Note);
            end if;
         end;
      end if;

      --  Search for any expansion modifier.
      --  We only search from the second character, because we need
      --  at least one character for the parameter name. On the other
      --  hand, a null word is allowed.
      for J in E'First + 1 .. E'Last loop
         if E (J) = '-' or else E (J) = '=' or else E (J) = '?'
           or else E (J) = '+' or else E (J) = '%' or else E (J) = '#'
         then
            Modifier := E (J);
            Parameter_Last := J - 1;
            Word_First := J + 1;
            exit;
         end if;
      end loop;

      --  No modifier, no expansion.
      if Modifier = ASCII.NUL then
         return Get_Var_Value (State.all, E, Note);
      end if;

      --  If the modifier is preceded by a colon, then it means we should
      --  treat null variables as unset.
      if E (Parameter_Last) = ':' then
         Treat_Null_As_Unset := True;
         Parameter_Last := Parameter_Last - 1;
      end if;

      --  check for suffix/prefix removal if this the largest or the smallest
      if Modifier = '%' and Word_First <= E'Last and E (Word_First) = '%' then
         Largest := True;
         Word_First := Word_First + 1;
      end if;

      if Modifier = '#' and Word_First <= E'Last and E (Word_First) = '#' then
         Largest := True;
         Word_First := Word_First + 1;
      end if;

      --  Return the expanded result...
      declare
         Word : constant Annotated_String := Slice (Expr, Word_First, E'Last);
         Result : constant Annotated_String :=
           Expanded_Parameter (State,
                               Param_Name => E (E'First .. Parameter_Last),
                               Word => Word,
                               Modifier => Modifier,
                               Treat_Null_As_Unset => Treat_Null_As_Unset,
                               Largest => Largest,
                               Note => Note);
      begin
         return Result;
      end;
   end Eval_Param_Expression;

   -----------------
   -- Eval_String --
   -----------------

   function Eval_String
     (SS : Shell_State_Access;
      S : Annotated_String;
      Max_Split : Integer := -1)
      return String_List
   is
      use Ada.Strings.Unbounded;
      use Dyn_String_Lists;

      Result : constant Annotated_String := Eval_String_Aux (SS, S);
      Result_List : Dyn_String_List;

      type State is
        (EMPTY_FIELD,
         FORCED_EMPTY_FIELD,
         NULL_WORD_FIELD,
         QUOTED_WORD_FIELD,
         WORD_FIELD);

      Current_State : State := EMPTY_FIELD;

      Buffer       : Unbounded_String := To_Unbounded_String ("");
      Split_Count  : Integer := 0;
      --  Number of fields encountered so far

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
      for I in Str (Result)'Range loop

         declare
            CC : constant Character := Get_Character (Result, I);
            CA : constant Annotation := Get_Annotation (Result, I);
         begin
            case CA is
               when FIELD_SEP =>
                  --  Force a field splitting even there is no character from
                  --  IFS. This is used to handle special expansion of $@ and
                  --  $*
                  Delimit_Word;
               when NULL_STRING =>
                  --  We have a null string character. If we are inside a word
                  --  then ignore it, otherwise the current field is currently
                  --  a null word that should not be ignored.
                  if Current_State = EMPTY_FIELD then
                     Current_State := NULL_WORD_FIELD;
                  end if;
               when QUOTED_NULL_STRING =>
                  --  In that case no empty word should be generated if the
                  --  word contains only QUOTED_NULL_STRING and NULL_STRING.
                  if Current_State = EMPTY_FIELD or else
                    Current_State = NULL_WORD_FIELD
                  then
                     Current_State := FORCED_EMPTY_FIELD;
                  end if;
               when UNSPLITABLE =>
                  --  The character is marked as unsplitable so don't check for
                  --  field splitting
                  Current_State := QUOTED_WORD_FIELD;
                  Buffer       := Buffer & CC;
               when others =>
                  if In_IFS (CC) and then
                    (Max_Split = -1 or else Split_Count < Max_Split)
                  then
                     Delimit_Word;
                  else
                     if Current_State /= QUOTED_WORD_FIELD then
                        Current_State := WORD_FIELD;
                     end if;
                     Buffer := Buffer & CC;
                  end if;
            end case;
         end;
      end loop;

      --  Get last characters from the buffer and delimit the last field
      Delimit_Word;

      --  if Last (Result_List) = 0 then
      --    return Null_Result;
      --  else
      return Content (Result_List);

      --  end if;
   end Eval_String;

   ---------------------
   -- Eval_String_Aux --
   ---------------------

   function Eval_String_Aux
     (SS : Shell_State_Access;
      S : Annotated_String;
      Case_Pattern : Boolean := False;
      Quote_Removal_Only : Boolean := False;
      Context : Annotation := NO_ANNOTATION)
      return Annotated_String
   is

      Buffer : Annotated_String;

      Index : Integer := 1;

      procedure Eval_Single_Quote;
      --  Eval a single quoted string (i.e of form 'xxxxx')

      procedure Eval_Param_Subst (A : Annotation);
      --  Eval a parameter substitution construction

      procedure Eval_Command_Subst (A : Annotation);
      --  Eval a command substitution construction

      procedure Eval_Double_Quote;
      --  Eval a double quoted string

      ------------------------
      -- Eval_Command_Subst --
      ------------------------

      procedure Eval_Command_Subst (A : Annotation) is
         Saved_Index : Integer;
      begin
         if Str (S) (Index) = '$' then
            Index := Index + 2;
         else
            Index := Index + 1;
         end if;

         Saved_Index := Index;
         loop
            exit when Get_Annotation (S, Index) = COMMAND_SUBST_END;
            Index := Index + 1;
         end loop;

         if Index > Saved_Index then
            declare
               T : Shell_Tree_Access;
            begin
               T := Parse_String (Str (S) (Saved_Index .. Index - 1));
               Append (Buffer, Strip (Eval (SS, T.all)), A);
               Free_Node (T);
            end;
         end if;
      end Eval_Command_Subst;

      -----------------------
      -- Eval_Double_Quote --
      -----------------------

      procedure Eval_Double_Quote is
         Saved_Buffer_Length : constant Integer := Length (Buffer);
      begin
         Index := Index + 1;

         loop
            if Get_Annotation (S, Index) = DOUBLE_QUOTE_END then
               if Length (Buffer) = Saved_Buffer_Length then
                  Append (Buffer, 'N', NULL_STRING);
               end if;
               exit;
            end if;

            case Get_Annotation (S, Index) is
               when COMMAND_SUBST_BEGIN =>
                  if Quote_Removal_Only then
                     Append (Buffer, Str (S) (Index), UNSPLITABLE);
                  else
                     Eval_Command_Subst (UNSPLITABLE);
                  end if;
               when PARAM_EVAL_BEGIN =>
                  if Quote_Removal_Only then
                     Append (Buffer, Str (S) (Index), UNSPLITABLE);
                  else
                     Eval_Param_Subst (UNSPLITABLE);
                  end if;
               when ESCAPE_SEQUENCE =>
                  Append (Buffer, Str (S) (Index), UNSPLITABLE);
               when QUOTED_NULL_STRING =>
                  --  We got the result of $@ with no positional parameters set
                  --  We should give that information to the part of the code
                  --  in charge of field splitting.
                  Append (Buffer, Str (S) (Index), QUOTED_NULL_STRING);
               when others =>
                  Append (Buffer, Str (S) (Index), UNSPLITABLE);
            end case;
            Index := Index + 1;
         end loop;
      end Eval_Double_Quote;

      ----------------------
      -- Eval_Param_Subst --
      ----------------------

      procedure Eval_Param_Subst (A : Annotation) is
         Param_First : Integer;
         Param_Last : Integer;

         Do_Expansion : Boolean := False;
         --  By default, we do not need to do parameter expansion.
         --  We do the expansion when we have "${expression}".
         Nesting_Counter : Integer := 0;
      begin
         --  Skip the initial '$'...
         Index := Index + 1;

         if Str (S) (Index) = '{' then
            Do_Expansion := True;
            Index := Index + 1;
         end if;
         Param_First := Index;

         loop
            if Get_Annotation (S, Index) = PARAM_EVAL_BEGIN then
               Nesting_Counter := Nesting_Counter + 1;

            elsif Get_Annotation (S, Index) = PARAM_EVAL_END then
               if Nesting_Counter = 0 then
                  exit;
               else
                  Nesting_Counter := Nesting_Counter - 1;
               end if;
            end if;

            Index := Index + 1;
         end loop;
         Param_Last := Index;

         if Str (S) (Param_Last) = '}' then
            Param_Last := Param_Last - 1;
         end if;

         if Param_Last >= Param_First then
            declare
               Expr : constant Annotated_String :=
                 Slice (S, Param_First, Param_Last);
            begin
               Append
                 (Buffer, Eval_Param_Expression (SS, Expr, Do_Expansion, A));
            end;
         end if;
      exception
         when Variable_Name_Error =>
            Error (SS.all, Str (S) & ": bad substitution");
            raise;
      end Eval_Param_Subst;

      -----------------------
      -- Eval_Single_Quote --
      -----------------------

      procedure Eval_Single_Quote is
         Saved_Buffer_Length : constant Integer := Length (Buffer);
      begin
         Index := Index + 1;
         loop
            if Get_Annotation (S, Index) = SINGLE_QUOTE_END then
               if Length (Buffer) = Saved_Buffer_Length then
                  Append (Buffer, 'N', NULL_STRING);
               end if;
               exit;
            end if;
            Append (Buffer, Str (S) (Index), UNSPLITABLE);
            Index := Index + 1;
         end loop;
      end Eval_Single_Quote;

   begin
      while Index <= Length (S) loop
         case Get_Annotation (S, Index) is
            when SINGLE_QUOTE_BEGIN => Eval_Single_Quote;
            when DOUBLE_QUOTE_BEGIN => Eval_Double_Quote;
            when COMMAND_SUBST_BEGIN =>
               if Quote_Removal_Only then
                  Append (Buffer, Str (S) (Index), Context);
               else
                  Eval_Command_Subst (Context);
               end if;
            when PARAM_EVAL_BEGIN =>
               if Quote_Removal_Only then
                  Append (Buffer, Str (S) (Index), Context);
               else
                  Eval_Param_Subst (Context);
               end if;
            when ESCAPE_SEQUENCE =>
               if not Case_Pattern then
                  Append (Buffer, Str (S) (Index), UNSPLITABLE);
               else
                  case Str (S) (Index) is
                     when '[' | ']' | '*' | '?' =>
                        Append (Buffer, '\', Context);
                        Append (Buffer, Str (S) (Index), Context);
                     when others =>
                        Append (Buffer, Str (S) (Index), Context);
                  end case;
               end if;
            when NO_ANNOTATION =>
               Append (Buffer, Str (S) (Index), Context);
            when PARAM_EVAL_END | COMMAND_SUBST_END =>
               if Quote_Removal_Only then
                  Append (Buffer, Str (S) (Index), Context);
               else
                  raise Program_Error;
               end if;
            when others =>
               raise Program_Error;
         end case;
         Index := Index + 1;
      end loop;

      return Buffer;
   end Eval_String_Aux;

   ----------------------
   -- Eval_String_List --
   ----------------------

   function Eval_String_List
     (SS : Shell_State_Access; S : Annotated_String_List) return String_List
   is

   begin
      if Length (S) = 1 then
         return Eval_String (SS, Element (S, 1));
      end if;

      declare
         use Dyn_String_Lists;
         Result : Dyn_String_List;

      begin
         if S = Null_Annotated_String_List then
            return Null_String_List;
         end if;

         for I in 1 .. Length (S) loop
            declare
               Elem_Eval : constant String_List :=
                 Eval_String (SS, Element (S, I));
            begin

               for J in Elem_Eval'Range loop
                  Append (Result, Elem_Eval (J));
               end loop;
            end;
         end loop;

         return Content (Result);
      end;
   end Eval_String_List;

   -------------------------
   -- Eval_String_Unsplit --
   -------------------------

   function Eval_String_Unsplit
     (SS : Shell_State_Access;
      S : Annotated_String;
      Case_Pattern : Boolean := False;
      Quote_Removal_Only : Boolean := False)
      return String
   is
      Result : constant Annotated_String := Eval_String_Aux
        (SS, S, Case_Pattern, Quote_Removal_Only);
      --  we create a buffer or twice the size as some characters might be
      --  excaped in case construct context
      Result_String : String (1 .. Length (Result) * 2);
      Result_Index  : Integer := 1;
   begin
      if Has_Null_String (Result) or else Case_Pattern then
         for I in 1 .. Length (Result) loop
            declare
               Current_Char : constant Character := Str (Result) (I);
            begin

               if Get_Annotation (Result, I) /= NULL_STRING and then
                 Get_Annotation (Result, I) /= QUOTED_NULL_STRING
               then
                  if Get_Annotation (Result, I) = UNSPLITABLE and then
                    Case_Pattern
                  then
                     case Current_Char is
                        when '*' | '?' | '[' | ']' =>
                           Result_String (Result_Index) := '\';
                           Result_Index := Result_Index + 1;
                        when others =>
                           null;
                     end case;
                  end if;
                  Result_String (Result_Index) := Current_Char;
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
   -- Expanded_Parameter --
   ------------------------

   function Expanded_Parameter (SS : Shell_State_Access;
                                Param_Name : String;
                                Word : Annotated_String;
                                Modifier : Character;
                                Treat_Null_As_Unset : Boolean;
                                Largest : Boolean;
                                Note : Annotation)
     return Annotated_String
   is
      Is_Set : Boolean := Is_Var_Set (SS.all, Param_Name);
      Parameter_Value : Annotated_String;
   begin
      if Is_Set then
         Parameter_Value := Get_Var_Value (SS.all, Param_Name, Note, False);
      end if;

      if Treat_Null_As_Unset and then
        Is_Set and then
        Length (Parameter_Value) = 0
      then
         Is_Set := False;
      end if;

      case Modifier is
         when '-' =>
            if not Is_Set then
               return Eval_String_Aux (SS, Word, Context => Note);
            else
               return Parameter_Value;
            end if;

         when '=' =>
            if not Is_Set then
               declare
                  Word_Value : constant Annotated_String :=
                    Eval_String_Aux (SS, Word, Context => Note);
               begin
                  Set_Var_Value (SS.all,
                                 Name => Param_Name,
                                 Value => Str (Word_Value));
                  return Word_Value;
               end;
            else
               return Parameter_Value;
            end if;

         when '?' =>
            if not Is_Set then
               declare
                  Word_Value : constant String :=
                    Eval_String_Unsplit (SS, Word);
               begin
                  if Word_Value /= "" then
                     Error (SS.all, Param_Name & ": " & Word_Value);
                  else
                     Error (SS.all,
                            Param_Name & ": parameter null or not set");
                  end if;
               end;
               Shell_Exit (SS.all, 1);
            else
               return Parameter_Value;
            end if;

         when '+' =>
            if Is_Set then
               return Eval_String_Aux (SS, Word, Context => Note);
            else
               return To_Annotated_String ("", Note);
            end if;
         when '%' =>
            --  Implement remove suffix
            declare
               Word_Value : constant String :=
                 Eval_String_Unsplit (SS, Word, Case_Pattern => True);

               P_Value : constant String := Str (Parameter_Value);
            begin
               if Largest then
                  for J in P_Value'Range loop
                     if Fnmatch (Word_Value, P_Value (J .. P_Value'Last)) then
                        return Slice (Parameter_Value, 1, J - 1);
                     end if;
                  end loop;
               else
                  for J in reverse P_Value'Range loop
                     if Fnmatch (Word_Value, P_Value (J .. P_Value'Last)) then
                        return Slice (Parameter_Value, 1, J - 1);
                     end if;
                  end loop;
               end if;

               return Parameter_Value;
            end;
         when '#' =>
            --  Implement remove prefix
            declare
               Word_Value : constant String :=
                 Eval_String_Unsplit (SS, Word, Case_Pattern => True);

               P_Value : constant String := Str (Parameter_Value);
            begin
               if not Largest then
                  for J in P_Value'Range loop
                     if Fnmatch (Word_Value, P_Value (P_Value'First .. J)) then
                        return Slice (Parameter_Value, J + 1,
                                      Length (Parameter_Value));
                     end if;
                  end loop;
               else
                  for J in reverse P_Value'Range loop
                     if Fnmatch (Word_Value, P_Value (P_Value'First .. J)) then
                        return Slice (Parameter_Value, J + 1,
                                     Length (Parameter_Value));
                     end if;
                  end loop;
               end if;

               return Parameter_Value;
            end;

         when others =>
            --  Impossible!
            pragma Assert (False);
            return To_Annotated_String ("", Note);
      end case;
   end Expanded_Parameter;

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
