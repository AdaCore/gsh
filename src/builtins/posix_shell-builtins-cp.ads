------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Cp                            --
--                                                                          --
--                                 S p e c                                  --
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

package Posix_Shell.Builtins.Cp is

   function Cp_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result;
   --  Implement the "cp" builtin.
   --  Deviation from Standard:
   --    * only -R (-r), -p and -f are currently currently supported.
   --    * Env variable such as LANG, LC_ALL, ... do not actually affect
   --      the execution of "cp"
   --    * WORKAROUND for Windows executables :
   --      `cp a b` when file 'a' does not actually exist, uses on a windows
   --      platform the file 'a.exe' (if existing) as source.
   --      the target 'b' will be named 'b.exe'

end Posix_Shell.Builtins.Cp;
