------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Cd                            --
--                                                                          --
--                                 B o d y                                  --
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

with Posix_Shell.Builtins.Support; use Posix_Shell.Builtins.Support;

package body Posix_Shell.Builtins.Cd is

   ------------------------
   -- Change_Dir_Builtin --
   ------------------------

   function Change_Dir_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
   begin
      --  If there was no argument provided, then cd to the HOME directory.
      --  If HOME directory is not provided, then the behavior is
      --  implementation-defined, and we simply do nothing.
      if Args'Length =  0 then
         return Change_Dir (S, Get_Var_Value (S, "HOME"));
      end if;

      --  "-" is a special case: It should be equivalent to
      --  ``cd "$OLDPWD" && pwd''
      if Args (Args'First).all = "-" then
         return Change_Dir
           (S, Get_Var_Value (S, "OLDPWD"), Verbose => True);
      end if;

      return Change_Dir (S, Args (Args'First).all);
   end Change_Dir_Builtin;

end Posix_Shell.Builtins.Cd;
