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

with Ada.Finalization;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Token_Lists is

   type Token_List is private;
   Null_Token_List : constant Token_List;

   procedure Append
     (Source : in out Token_List; Item : Token);

   procedure Append
     (Source : in out Token_List; Item : Token_List);

   function "&" (Left : Token;
                 Right : Token_List)
                 return Token_List;

   function Length (Source : Token_List) return Integer;

   function Element
     (Source : Token_List;
      Index : Natural)
      return Token;

private
   type Node;
   type Node_Access is access Node;

   type Node is record
      T      : Token;
      Next   : Node_Access := null;
   end record;

   type Token_List is record
      First  : Node_Access := null;
      Last   : Node_Access := null;
      Length : Natural := 0;
   end record;

   Null_Token_List : constant Token_List := (null, null, 0);

end Token_Lists;
