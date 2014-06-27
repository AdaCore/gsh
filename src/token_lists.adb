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

package body Token_Lists is

   -------
   -- & --
   -------

   function "&"
     (Left  : Token;
      Right : Token_List)
      return Token_List
   is
      Result : Token_List := Null_Token_List;
   begin
      Append (Result, Left);
      Append (Result, Right);
      return Result;
   end "&";

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Token_List; Item : Token)
   is
      N : constant Node_Access := new Node'(Item, null);
   begin
      Source.Length := Source.Length + 1;
      if Source.First /= null then
         Source.Last.Next := N;
         Source.Last := N;
      else
         Source.First := N;
         Source.Last := N;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source : in out Token_List;
      Item   : Token_List)
   is
   begin
      if Item.Length > 0 then
         Source.Length := Source.Length + Item.Length;
         if Source.First /= null then
            Source.Last.Next := Item.First;
            Source.Last := Item.Last;
         else
            Source.First := Item.First;
            Source.Last := Item.Last;
         end if;
      end if;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Token_List;
      Index  : Natural)
      return Token
   is
      N : Node_Access := Source.First;
   begin
      if Index = 1 then
         return N.T;
      else
         for J in 1 .. Index - 1 loop
            N := N.Next;
         end loop;
         return N.T;
      end if;
   end Element;

   ------------
   -- Length --
   ------------

   function Length (Source : Token_List) return Integer
   is
   begin
      return Source.Length;
   end Length;

end Token_Lists;
