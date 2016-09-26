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

--  This package implements a lexer that recognize the tokens of the POSIX
--  shell (Bourne Shell syntax type). IEEE Std 1003.1, 2004 Edition (Chapter 2)

--  The token recognition is defined by a set of rules in section 2.3.

with Sh.Buffers; use Sh.Buffers;

package Sh.Tokens is

   ------------
   -- Tokens --
   ------------

   type Token is private;
   --  T is the type of the token. If T = T_WORD then S contains the word.
   --  Lineno and Columnno are the position of the token in the file.

   Null_Token  : constant Token;

   function Kind (T : Token) return Token_Type;
   --  Get token type
   --
   --  @param T a token
   --  @return the token type

   function As_String (T : Token) return String;
   --  Get the string representing the token
   --
   --  @param T a token
   --  @return string representing the token

   function Get_Token_Pos (T : Token) return Text_Position;
   --  Get the position in the buffer of the token
   --
   --  @param T a token
   --  @return a text position object

   function Image (T : Token_Type) return String;
   --  Return a string image of a token type
   --
   --  This function is used mainly for error and debug messages computation.
   --
   --  @param T a token
   --  @return a string

   function Image (T : Token) return String;
   --  Return a string image of a token type
   --
   --  This function is used mainly for error and debug messages computation.
   --  The string contains position, token type and content for T_WORD tokens
   --
   --  @param T a token
   --  @return a string

   ------------------
   -- Token_Buffer --
   ------------------

   procedure Syntax_Error (T : Token; Msg : String);
   --  Raise a Shell_Lexer_Error
   --
   --  @param T a token
   --  @param Msg a message associated with the exception

   Shell_Lexer_Error    : exception;
   --  error raised in case of error during lexing

   -------------------
   --  Redirections --
   -------------------

   type Operator is
     (NULL_REDIR,   --  no redirection
      OPEN_READ,    --  input redirection (i.e: <)
      OPEN_WRITE,   --  output redirection (i.e: >)
      OPEN_APPEND,  --  ouput redirection wihtout cloberring (i.e: >>)
      DUPLICATE,    --  file handler duplication (i.e: [n1]>&[n2])
      IOHERE);      --  here document declaration (i.e: << and <<-)

   type Redirection (Kind : Operator := NULL_REDIR) is record
      case Kind is
         when NULL_REDIR =>
            null;
         when others =>
            Target_Fd : Natural; --  File descriptor that should be updated
            case Kind is
               when OPEN_READ | OPEN_WRITE | OPEN_APPEND  =>
                  Filename    : Token;
                  --  Path to file that should be associated with target fd
               when DUPLICATE =>
                  Source      : Token;
                  --  File descriptor that should be duplicated as Target_Fd
               when IOHERE =>
                  --  Content associated to Target_Fd
                  Content     : Token;
                  --  If True shell expansions/substitutions should be done on
                  --  the content.
                  Expand      : Boolean;
               when NULL_REDIR =>
                  null;
            end case;
      end case;
   end record;
   --  Record representing a redirection directive

   type Redirection_Stack is private;

   Empty_Redirections : constant Redirection_Stack;

   procedure Push
     (RS : in out Redirection_Stack;
      R  : Redirection);

   function Length (RS : Redirection_Stack) return Natural;

   function Element
     (RS    : Redirection_Stack;
      Index : Positive)
      return Redirection;

   pragma Inline (Kind);

private
   type Token_Stack is array (1 .. 16) of Token;

   type Token is record
      Kind     : Token_Type := T_WORD;
      First    : Text_Position;
      Last     : Text_Position;
      Content  : Buffer;
   end record;

   Null_Token : constant Token := (T_WORD,
                                   Null_Text_Position,
                                   Null_Text_Position,
                                   Null_Buffer);

   type Redirection_Array is array (1 .. 16) of Redirection;
   type Redirection_Stack is record
      Top : Natural := 0;
      Ops : Redirection_Array;
   end record;
   --  Redirection directives for Stdin, Stdout and Stderr.

   Empty_Redirections : constant Redirection_Stack :=
     (0, (others => (Kind => NULL_REDIR)));

end Sh.Tokens;
