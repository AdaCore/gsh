------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Support                       --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.Directory_Operations;

package body Posix_Shell.Builtins.Support is

      -------------------------
      --  Recursive_Make_Dir --
      -------------------------

      procedure Recursive_Make_Dir (Dir : String) is
      begin
         if Is_Directory (Dir) then
            return;
         else
            declare
               PD : constant String :=
                 GNAT.Directory_Operations.Dir_Name (Dir);
               PD_Last : Integer := PD'Last;
            begin
               if not Is_Directory (PD) then
                  if PD (PD'Last) = '\' or else PD (PD'Last) = '/' then
                     PD_Last := PD_Last - 1;
                  end if;

                  if PD_Last >= PD'First then
                     Recursive_Make_Dir (PD (PD'First .. PD_Last));
                  end if;
               end if;
            end;
            GNAT.Directory_Operations.Make_Dir (Dir);
         end if;
      end Recursive_Make_Dir;

end Posix_Shell.Builtins.Support;
