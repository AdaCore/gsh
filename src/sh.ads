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

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

package Sh is

   type Annotation is
     (NULL_STRING,           --  a '' string (character value is ignored)
      QUOTED_NULL_STRING,    --  a '' string but that should also be ignored
                             --  even inside a quote. This state is used only
                             --  in relation with "$@" expansion
      UNSPLITABLE_BEGIN,     --  character cannot be a field separator
      UNSPLITABLE_END,
      FIELD_SEP);            --  character must be a field separator
   --  List of possible annotations.

   type Annotations is array (Natural range <>) of Annotation;
   --  Annotations arrays

   type Annotation_Record is record
      A : Annotation;
      Length : Natural;
   end record;

   type Annotation_Records is array (Natural range <>) of Annotation_Record;

   type Token_Type is
     (T_NULL,
      T_WORD,         -- word token
      T_EOF,          -- end of file token
      T_ASSIGNEMENT,  -- variable assignement token

      --  Operators

      T_DSEMI,        -- ';;'
      T_AND_IF,       -- '&&'
      T_AND,          -- '&'
      T_OR_IF,        -- '||'
      T_PIPE,         -- '|'

      --  Output redirection operators

      T_DGREAT,       -- '>>'
      T_CLOBBER,      -- '>|'
      T_GREATAND,     -- '>&'
      T_GREAT,        -- '>'

      --  Input redirection operators

      T_LESS,         -- '<'
      T_DLESS,        -- '<<'
      T_DLESSDASH,    -- '<<-'
      T_LESSAND,      -- '<&'
      T_LESSGREAT,    -- '<>'

      T_IO_NUMBER,

      T_NEWLINE,      -- LF
      T_LPAR,         -- '('
      T_SEMI,         -- ';'
      T_RPAR,         -- ')'

      --  Keywords

      T_IF,           -- 'if'
      T_THEN,         -- 'then'
      T_ELSE,         -- 'else'
      T_ELIF,         -- 'elif'
      T_FI,           -- 'fi'
      T_DO,           -- 'do'
      T_DONE,         --  'done'
      T_BANG,         -- '!'
      T_IN,           -- 'in'
      T_CASE,         -- 'case'
      T_ESAC,         -- 'esac'
      T_WHILE,        -- 'while'
      T_UNTIL,        -- 'until'
      T_FOR,          -- 'for'
      T_LBRACE,       -- '}'
      T_RBRACE       -- '{'
      );   -- number preceding a redirection operator

   subtype Input_Redirection_Ops is Token_Type range T_LESS .. T_LESSGREAT;
   subtype Output_Redirection_Ops is Token_Type range T_DGREAT .. T_GREAT;
   subtype Redirection_Ops is Token_Type range T_DGREAT .. T_LESSGREAT;
   subtype Redirection_Tokens is Token_Type range T_DGREAT .. T_IO_NUMBER;

   Is_Windows : constant Boolean :=
     GNAT.Directory_Operations.Dir_Separator = '\';

   Shell_Syntax_Error    : exception;
   Shell_Non_Implemented : exception;
   Buffer_Read_Error : exception;

   Shell_Exit_Exception : exception;
   --  An exception signaling that we need to exit the current shell.
   --  At the time when this exception is raised, the exit status has
   --  already been saved. The exception mechanism is used only during shell
   --  substitution.

   Null_String_List : GNAT.OS_Lib.String_List (1 .. 0);

   Null_String_List_Access : GNAT.OS_Lib.String_List_Access :=
     new GNAT.OS_Lib.String_List'(Null_String_List);

   subtype Node_Id is Natural;
   Null_Node : constant Node_Id := 0;

   type Result_Kind is
     (RESULT_STD,
      RESULT_EXIT,
      RESULT_RETURN,
      RESULT_BREAK,
      RESULT_CONTINUE);

   type Eval_Result (Kind : Result_Kind := RESULT_STD) is record
      case Kind is
         when RESULT_STD | RESULT_EXIT | RESULT_RETURN =>
            Status : Integer;
         when RESULT_BREAK | RESULT_CONTINUE =>
            Level : Integer;
      end case;
   end record;

   Continue_Exception : exception;
   Break_Exception : exception;

   --  The following declaration ensure that automatic expansion of arguments
   --  done by GNAT is disabled. Note that on compiler older than 2016-02-17
   --  the declaration has no effect.
   Do_Args_Expansion : Integer := 0;
   pragma Export (C, Do_Args_Expansion, "__gnat_do_argv_expansion");
end Sh;
