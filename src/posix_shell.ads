with GNAT.OS_Lib;

package Posix_Shell is

   type Annotation is
     (NO_ANNOTATION,         --  no annotation
      SINGLE_QUOTE_BEGIN,    --  mark beginning of a single quote expression
      SINGLE_QUOTE_END,      --  mark end of a single quote expression
      DOUBLE_QUOTE_BEGIN,    --  likewise for double quotes
      DOUBLE_QUOTE_END,
      ESCAPE_SEQUENCE,       --  mark an escaped character
      COMMAND_SUBST_BEGIN,   --  mark beginning of a command substitution
      COMMAND_SUBST_END,     --  mark end of a command substitution
      PARAM_EVAL_BEGIN,      --  likewise for parameter evaluation
      PARAM_EVAL_END,
      NULL_STRING,           --  a '' string (character value is ignored)
      UNSPLITABLE,           --  character cannot be a field separator
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
