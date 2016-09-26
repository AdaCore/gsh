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

with Ada.Exceptions; use Ada.Exceptions;

package body Sh.Tokens is

   -------------
   -- Element --
   -------------

   function Element
     (RS    : Redirection_Stack;
      Index : Positive)
      return Redirection
   is
   begin
      return RS.Ops (Index);
   end Element;

   ----------
   -- Kind --
   ----------

   function Kind (T : Token) return Token_Type is
   begin
      return T.Kind;
   end Kind;

   ---------------
   -- As_String --
   ---------------

   function As_String (T : Token) return String is
   begin
      return Slice (T.Content, T.First, T.Last);
   end As_String;

   -------------------
   -- Get_Token_Pos --
   -------------------

   function Get_Token_Pos (T : Token) return Text_Position is
   begin
      return T.First;
   end Get_Token_Pos;

   ------------
   -- Length --
   ------------

   function Length (RS : Redirection_Stack) return Natural is
   begin
      return RS.Top;
   end Length;

   ----------
   -- Push --
   ----------

   procedure Push
     (RS : in out Redirection_Stack;
      R  : Redirection)
   is
   begin
      --  ??? missing overflow check ?
      RS.Top := RS.Top + 1;
      RS.Ops (RS.Top) := R;
   end Push;

   ------------------
   -- Syntax_Error --
   ------------------

   procedure Syntax_Error (T : Token; Msg : String) is
   begin
      if T.Kind = T_WORD then
         Raise_Exception
           (Shell_Syntax_Error'Identity,
            Image (T.First) & ":"
            & Msg & " (got '"
            & Image (T.Kind) & "',"
            & Slice (T.Content, T.First, T.Last) & ")");
      else
         Raise_Exception
           (Shell_Syntax_Error'Identity,
            Image (T.First) & ":"
            & Msg & " (got '"
            & Image (T.Kind) & "')");
      end if;

   end Syntax_Error;

   -----------
   -- Image --
   -----------

   function Image (T : Token) return String is
   begin
      if T.Kind = T_WORD then
         return Image (T.First) & ":" & Image (T.Kind)
           & "(" & Slice (T.Content, T.First, T.Last) & ")";
      else
         return Image (T.First) & ":" & Image (T.Kind);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      case T is
         when T_WORD        => return "word";
         when T_EOF         => return "end of file";
         when T_ASSIGNEMENT => return "assignement";
         when T_DSEMI       => return ";;";
         when T_AND_IF      => return "&&";
         when T_AND         => return "&";
         when T_OR_IF       => return "||";
         when T_PIPE        => return "|";
         when T_DGREAT      => return ">>";
         when T_CLOBBER     => return ">|";
         when T_GREATAND    => return ">&";
         when T_DLESSDASH   => return "<<-";
         when T_LESS        => return "<";
         when T_GREAT       => return ">";
         when T_DLESS       => return "<<";
         when T_LESSAND     => return "<&";
         when T_LESSGREAT   => return "<>";
         when T_NEWLINE     => return "<LF>";
         when T_LPAR        => return "(";
         when T_SEMI        => return ";";
         when T_RPAR        => return ")";
         when T_IF          => return "if";
         when T_THEN        => return "then";
         when T_ELSE        => return "else";
         when T_ELIF        => return "elif";
         when T_FI          => return "fi";
         when T_DO          => return "do";
         when T_DONE        => return "done";
         when T_BANG        => return "!";
         when T_IN          => return "in";
         when T_CASE        => return "case";
         when T_ESAC        => return "esac";
         when T_WHILE       => return "while";
         when T_UNTIL       => return "until";
         when T_FOR         => return "for";
         when T_LBRACE      => return "{";
         when T_RBRACE      => return "}";
         when others        => return T'Img;
      end case;
   end Image;

end Sh.Tokens;
