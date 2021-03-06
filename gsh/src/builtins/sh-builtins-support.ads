------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Support                       --
--                                                                          --
--                                 S p e c                                  --
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

package Sh.Builtins.Support is

   procedure Recursive_Make_Dir (Dir : String);

   function Change_Dir
     (S : in out Shell_State;
      Dir_Name : String;
      Verbose : Boolean := False)
     return Integer;
   --  Change the directory to Dir_Name and return 0 if successful.
   --  This function also maintains the PWD and OLDPWD variables.
   --  If Verbose is True and the directory change was successful,
   --  then print on standard output the name of the new directory.
   --
   --  This function does nothing and returns zero if Dir_Name is
   --  the empty string.

end Sh.Builtins.Support;
