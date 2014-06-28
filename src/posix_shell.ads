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

with GNAT.OS_Lib;

package Posix_Shell is

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
      T_DLESSDASH,    -- '>>-'

      --  Output redirection operators

      T_DGREAT,       -- '>>'
      T_CLOBBER,      -- '>|'
      T_GREATAND,     -- '>&'
      T_GREAT,        -- '>'

      --  Input redirection operators

      T_LESS,         -- '<'
      T_DLESS,        -- '<<'
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

   Shell_Syntax_Error    : exception;
   Shell_Non_Implemented : exception;
   Buffer_Read_Error : exception;

   Null_String_List : GNAT.OS_Lib.String_List (1 .. 0);

   Null_String_List_Access : GNAT.OS_Lib.String_List_Access :=
     new GNAT.OS_Lib.String_List'(Null_String_List);

   subtype Node_Id is Natural;
   Null_Node : constant Node_Id := 0;
end Posix_Shell;
