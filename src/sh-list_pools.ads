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

with GNAT.Dynamic_Tables;
with Sh.Lexer; use Sh.Lexer;

package Sh.List_Pools is

   subtype Token_List is Natural;
   --  Reference to a list in a pool

   type List_Pool is private;
   --  Pool of of token lists

   Null_List : constant Token_List := 0;
   --  An empty list

   procedure Append
     (Pool   : in out List_Pool;
      Source : in out Token_List;
      Item   : Token);
   --  Append a token to a list. Source contains the new list index

   procedure Append
     (Pool   : in out List_Pool;
      Source : in out Token_List;
      Item   : Token_List);
   --  Append a list to list. Source contains the new list index

   function Prepend
     (Pool   : in out List_Pool;
      Source : Token_List;
      Item   : Token)
     return Token_List;
   --  Prepend a token to a list

   procedure Deallocate (Pool : in out List_Pool; Source : Token_List);
   --  Free a list from the pool

   function Next (Pool : List_Pool; Source : Token_List) return Token_List;
   --  Get next element of a list

   function Get_Element (Pool : List_Pool; Source : Token_List) return Token;
   --  Get token of the first element in the list Source

   function Is_Empty (Pool : List_Pool; Source : Token_List) return Boolean;
   --  Return true if the list is the empty list

   function New_Pool return List_Pool;
   --  Create a new list pool

   procedure Deallocate (Pool : in out List_Pool);
   --  Deallocate all memory used by a pool

private

   type Node is record
      T      : Token := Null_Token;
      Next   : Token_List := 0;
      Last   : Token_List := 0;
   end record;

   package List_Pools is new GNAT.Dynamic_Tables
     (Node, Token_List, 1, 32, 100);
   type List_Pool is new List_Pools.Instance;

end Sh.List_Pools;
