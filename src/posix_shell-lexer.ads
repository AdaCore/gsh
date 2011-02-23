--  This package implements a lexer that recognize the tokens of the POSIX
--  shell (Bourne Shell syntax type). IEEE Std 1003.1, 2004 Edition (Chapter 2)

--  The token recognition is defined by a set of rules in section 2.3.

with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Posix_Shell.Buffers; use Posix_Shell.Buffers;

package Posix_Shell.Lexer is

   type Token is private;
   --  T is the type of the token. If T = T_WORD then S contains the word.
   --  Lineno and Columnno are the position of the token in the file.

   function Get_Token_Type (T : Token) return Token_Type;
   --  return type of the token T

   function Get_Token_String (T : Token) return Annotated_String;
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

   function Read_Token (B : Buffer_Access) return Token;
   --  Get the next token in the buffer B. Keywords are not handled. If a
   --  keyword, is encountered then a token T_WORD will be returned.

   function Read_Token (B : Buffer_Access) return Token_Type;
   --  Same as previous function except that only the type is returned.

   function Read_Token (B : Buffer_Access) return Annotated_String;
   --  Same as above except that only the associated string is returned

   function Read_Word_Token (B : Buffer_Access) return Annotated_String;

   procedure Expect_Token
     (B   : Buffer_Access;
      T   : Token_Type;
      Msg : String := "");
   --  Get the next token in the buffer B. If its type is not T then raise a
   --  Shell_Syntax_Error exception.

   function Read_Command_Token (B : Buffer_Access) return Token;
   --  Get the next token in the buffer B with keyword recognition enabled.

   function Read_Command_Token (B : Buffer_Access) return Token_Type;
   --  Same as previous function except that only the type is returned

   procedure Skip_Token (B : Buffer_Access);
   --  Skip token

   function Lookahead (B : Buffer_Access) return Token_Type;
   --  Same as Read_Token function except that the buffer state is not modified

   function Lookahead_Command (B : Buffer_Access) return Token_Type;
   --  Same as Read_Command_Token except that the buffer state is not modified

   function Read_IOHere
     (B : Buffer_Access; Marker : Annotated_String) return Annotated_String;
   --  Read IO Here content

   function Token_Type_Img (T : Token_Type) return String;
   --  return a string represanting the current token type. this function
   --  is used mainly for error messages computation.

   function Token_Pos_Img (T : Token) return String;

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
      T        : Token_Type       := T_WORD;
      S        : Annotated_String := Null_Annotated_String;
      Pos      : Text_Position;
   end record;

end Posix_Shell.Lexer;
