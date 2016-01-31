------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Support                       --
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

with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

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

         begin
            GNAT.Directory_Operations.Make_Dir (Dir);
         exception
            when others =>
               --  We got an error while trying to create a directory. Due to
               --  the implementation, it might mean that the directory was
               --  created by another process in the meantime. In that case,
               --  don't raise an exception.
               if not Is_Directory (Dir) then
                  raise;
               end if;
         end;
      end if;
   end Recursive_Make_Dir;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (S        : in out Shell_State;
      Dir_Name : String;
      Verbose  : Boolean := False)
      return Integer
   is
      function Get_Absolute_Path (D : String) return String;

      function Get_Absolute_Path (D : String) return String is
      begin
         if Is_Absolute_Path (D) then
            return D;
         else
            return Get_Current_Dir (S) & "/" & D;
         end if;
      end Get_Absolute_Path;

      Abs_Dir : constant String := Get_Absolute_Path (Dir_Name);

   begin
      if Dir_Name = "" then
         return 0;
      end if;

      if not Is_Directory (Abs_Dir) then
         Put (S, 2, "cd: " & Dir_Name & ": No such file or directory");
         New_Line (S, 2);
         return 1;
      end if;

      declare
         use GNAT.Directory_Operations;
         Full_Path : constant String := Format_Pathname
           (GNAT.OS_Lib.Normalize_Pathname (Abs_Dir),
            GNAT.Directory_Operations.UNIX);
      begin
         Set_Current_Dir (S, Full_Path);
         Set_Var_Value (S, "OLDPWD", Get_Var_Value (S, "PWD"));

         --  Update PWd variable. Use a Unix format (without drive letter)
         Set_Var_Value (S, "PWD", Get_Current_Dir (S, True));
      end;

      if Verbose then
         Put (S, 1, Dir_Name);
         New_Line (S, 1);
      end if;

      return 0;

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         Put (S, 2,
              "cd: " & Dir_Name & ": Cannot change to this directory");
         New_Line (S, 2);
         return 1;
   end Change_Dir;

end Posix_Shell.Builtins.Support;
