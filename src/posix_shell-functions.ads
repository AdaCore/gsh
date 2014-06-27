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

with GNAT.Strings; use GNAT.Strings;

with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Functions is

   procedure Register_Function (Name : String; Tree : Shell_Tree);
   --  Register the given Node as a new function.

   function Is_Function (Name : String) return Boolean;
   --  Return True if a function of the given name has been defined.

   procedure Execute_Function
     (State : Shell_State_Access;
      Name  : String;
      Args  : String_List);
   --  Execute the given function with the provide arguments, and
   --  return the function exit status.
   --
   --  The behavior is unspecified if the function has not been
   --  previously defined.

end Posix_Shell.Functions;
