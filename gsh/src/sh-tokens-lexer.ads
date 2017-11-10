------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

package Sh.Tokens.Lexer is

   type Token_Buffer is private;
   --  Structure holding the state of the lexer.
   --  ??? rename to Lexer_State ???

   type Buffer_Access is access Token_Buffer;
   --  Access to a lexer state.
   --  ??? Providing this is a fixed sized structure we can probably get rid
   --  of access need and use in out parameters whenever it makes sense ???

   function New_Buffer_From_File (Filename : String) return Token_Buffer;
   --  Create a new lexer state from file content
   --
   --  @param Filename path to a script file
   --  @return a lexer state

   function New_Buffer (Str : String) return Token_Buffer;
   --  Create a new lexer state from string
   --
   --  @param Str a shell script
   --  @return a lexer state

   procedure Deallocate (B : in out Buffer_Access);

   procedure Deallocate (B : in out Token_Buffer);

   function Get_Buffer (B : Token_Buffer) return Buffer;
   --  Retrieve buffer associated with a lexer state
   --
   --  @param B a lexer state
   --  @return the buffer holding the associated string

   function Read_Token (B : in out Token_Buffer) return Token;
   --  Get the next token
   --
   --  Return the next token and move lexer position to next token. This
   --  function does not handle keywords (T_WORD token is returned instead)
   --
   --  @param a lexer state
   --  @return next token in the buffer

   function Read_Command_Token (B : in out Token_Buffer) return Token;
   --  Get the next command token
   --
   --  Same interface as Read_Token except that keywords are recognised
   --
   --  @param a lexer state
   --  @return next command token in the buffer

   function Read_Token (B : in out Token_Buffer) return Token_Type;
   --  Same as Read_Token except that only the type is returned.
   --
   --  @param B a lexer state
   --  @return token type of next token

   function Read_Token (B : in out Token_Buffer) return String;
   --  Same as Read_Token except that only the associated string is returned
   --
   --  @param B a lexer state
   --  @return string matched by the next token

   function Read_Word_Token (B : in out Token_Buffer) return Token;
   --  Read next token and expect a T_WORD token
   --
   --  @param B a lexer state
   --  @return the next token in the buffer
   --  @raise Shell_Syntax_Error in the case the next token is not a T_WORD one

   procedure Expect_Token
     (B   : in out Token_Buffer;
      T   : Token_Type;
      Msg : String := "");
   --  Assert that next token is of type T
   --
   --  Note that lexer state is moved forward
   --
   --  @param B a lexer state
   --  @param T expected token type
   --  @param Msg error message in case the next token kind is not T
   --  @raise Shell_Syntax_Error in case next token is not of type T

   procedure Skip_Token (B : in out Token_Buffer);
   --  Skip next token
   --
   --  @param B a lexer state

   function Lookahead (B : in out Token_Buffer) return Token_Type;
   --  Get token kind of upcoming token
   --
   --  Same as Read_Token function except that the buffer state is not modified
   --
   --  @param B a lexer state
   --  @return token kind of upcoming token

   function Lookahead_Command (B : in out Token_Buffer) return Token_Type;
   --  Get token kind of upcoming token including keywords
   --
   --  Same as Read_Command_Token function except that the buffer state is not
   --  modified
   --
   --  @param B a lexer state
   --  @return token kind of upcoming token

   pragma Inline (Read_Token);
   pragma Inline (Read_Command_Token);
   pragma Inline (Skip_Token);
   pragma Inline (Lookahead_Command);
   pragma Inline (Lookahead);

private

   type Token_Buffer is record
      Content                 : Buffer;
      --  Buffer containing the shell script

      Pushback_Loc            : Text_Position;
      --  Buffer location reached when pushback is called

      Cache_Loc               : Text_Position;
      --  Buffer location for the buffer if cache is consumed

      Cache_Token             : Token;
      --  Cached token

      Here_Doc_Stack          : Token_Stack;
      Here_Doc_Next           : Positive      := 1;
      Pending_Here_Docs       : Natural       := 0;
      Next_Is_Marker          : Boolean       := False;
      --  True is next token will be a here document marker

      Next_Is_Here_Docs       : Boolean       := False;
      --  True if next tokens are here documents

      Valid_Cache             : Boolean       := False;
   end record;
   --  S contains the content of the buffer, POS is the current position in the
   --  buffer. Current_Line and Current_Column is the current position in file
   --  (x,y). Previous_Token_* fields contain the information of the previous
   --  state of the buffer (one read_token backward).
end Sh.Tokens.Lexer;
