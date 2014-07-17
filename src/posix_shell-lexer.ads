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

--  This package implements a lexer that recognize the tokens of the POSIX
--  shell (Bourne Shell syntax type). IEEE Std 1003.1, 2004 Edition (Chapter 2)

--  The token recognition is defined by a set of rules in section 2.3.

with Posix_Shell.Buffers; use Posix_Shell.Buffers;

package Posix_Shell.Lexer is

   Debug_Lexer : Boolean := False;

   type Token is private;
   --  T is the type of the token. If T = T_WORD then S contains the word.
   --  Lineno and Columnno are the position of the token in the file.

   Null_Token  : constant Token;

   function Get_Token_Type (T : Token) return Token_Type;
   --  return type of the token T

   function Get_Token_String (T : Token) return String;
   --  return token string

   function Get_Token_Pos (T : Token) return Text_Position;
   --  return token position

   type Token_Buffer is record
      B                       : Buffer;
      Previous_Token_Pos      : Text_Position;
      Next_Token_Pos          : Text_Position;
      Next_Token              : Token;
      Valid_Cache             : Boolean       := False;
   end record;
   --  S contains the content of the buffer, POS is the current position in the
   --  buffer. Current_Line and Current_Column is the current position in file
   --  (x,y). Previous_Token_* fields contain the information of the previous
   --  state of the buffer (one read_token backward).

   type Buffer_Access is access Token_Buffer;
   --  Access to the buffer

   procedure Deallocate (B : in out Buffer_Access);

   function New_Buffer_From_File (Filename : String) return Token_Buffer;
   function New_Buffer (Str : String) return Token_Buffer;

   function Read_Token (B : in out Token_Buffer) return Token;
   --  Get the next token in the buffer B. Keywords are not handled. If a
   --  keyword, is encountered then a token T_WORD will be returned.

   function Read_Token (B : in out Token_Buffer) return Token_Type;
   --  Same as previous function except that only the type is returned.

   function Read_Token (B : in out Token_Buffer) return String;
   --  Same as above except that only the associated string is returned

   function Read_Word_Token (B : in out Token_Buffer) return Token;

   procedure Expect_Token
     (B   : in out Token_Buffer;
      T   : Token_Type;
      Msg : String := "");
   --  Get the next token in the buffer B. If its type is not T then raise a
   --  Shell_Syntax_Error exception.

   function Read_Command_Token (B : in out Token_Buffer) return Token;
   --  Get the next token in the buffer B with keyword recognition enabled.

   procedure Skip_Token (B : in out Token_Buffer);
   --  Skip token

   function Lookahead (B : in out Token_Buffer) return Token_Type;
   --  Same as Read_Token function except that the buffer state is not modified

   function Lookahead_Command (B : in out Token_Buffer) return Token_Type;
   --  Same as Read_Command_Token except that the buffer state is not modified

   function Read_IOHere
     (B      : in out Token_Buffer;
      Marker : Token;
      Eval   : out Boolean)
      return Token;
   --  Read IO Here content

   function Image (T : Token_Type) return String;
   --  return a string represanting the current token type. this function
   --  is used mainly for error messages computation.

   function Image (T : Token) return String;
   --  return string image of the token (the image contains position, type and
   --  content if the token is of type T_WORD.

   procedure Syntax_Error (T : Token; Msg : String);

   Shell_Lexer_Error    : exception;

   pragma Inline (Read_Token);
   pragma Inline (Get_Token_Type);
   pragma Inline (Read_Command_Token);
   pragma Inline (Skip_Token);
   pragma Inline (Lookahead_Command);
   pragma Inline (Lookahead);

private

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


end Posix_Shell.Lexer;
