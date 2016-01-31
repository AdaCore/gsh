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

with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.Parser is

   function Parse_String
     (S : String)
      return Shell_Tree;
   --  Same as above except that source is a string.

   function Parse_File (Filename : String) return Shell_Tree;
   --  Same as above except that source is the content of file Filename.

   function Parse_Buffer
     (B           : in out Token_Buffer;
      Until_Token : Token_Type := T_NULL)
      return Shell_Tree;
   --  Same as above except that the source is directly a buffer.

end Posix_Shell.Parser;
