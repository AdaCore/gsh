------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Cp                            --
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

with Ada.Directories;
with Interfaces.C;                 use Interfaces.C;
with GNAT.Directory_Operations;

with Posix_Shell.Rm;               use Posix_Shell.Rm;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Cp is

   ----------------
   -- Cp_builtin --
   ----------------

   --  treatment of options:
   --  `-f` is not actually used. Files are always removed if possible.
   --      users rights are not taken into account.
   --  `-p` changes the preservation of copy from 'Time_stamp' to 'Full'
   --      when using runtime 'Copy_File'

   function Cp_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is

      File_List_Start : Integer := Args'First;
      File_List_End   : constant Integer := Args'Last - 1;

      Target_Name     : constant String := (if File_List_End < File_List_Start
         then "" else
         GNAT.OS_Lib.Normalize_Pathname (Resolve_Path (S.all,
                                                       Args (Args'Last).all),
                                         Resolve_Links => False));

      Target_Dir_Exists : Boolean   := False;
      Recursive         : Boolean   := False;
      Force             : Boolean   := False;
      Preserve          : Attribute := Time_Stamps;
      Got_Errors        : Boolean   := False;

      procedure Cp_Tree (Filename : String);

      -------------
      -- Cp_Tree --
      -------------

      procedure Cp_Tree (Filename : String) is
         Search  : Ada.Directories.Search_Type;
         Dir_Ent : Ada.Directories.Directory_Entry_Type;
         Success : Boolean := False;
         Status  : long := 0;

         procedure Recursive_Copy (Source_Path : String;
                                   Target_Path : String);
         --------------------
         -- Recursive_Copy --
         --------------------

         procedure Recursive_Copy (Source_Path : String;
                                   Target_Path : String) is
            Success : Boolean := False;
         begin
            GNAT.Directory_Operations.Make_Dir (Target_Path);
            Ada.Directories.Start_Search (Search,
                                          Directory => Source_Path,
                                          Pattern   => "");

            while Ada.Directories.More_Entries (Search) loop
               Ada.Directories.Get_Next_Entry (Search, Dir_Ent);

               declare
                  Base_Name : constant String :=
                                Ada.Directories.Simple_Name (Dir_Ent);
                  Source    : constant String :=
                                Filename &
                                GNAT.Directory_Operations.Dir_Separator &
                                Base_Name;
                  Target    : constant String :=
                                Target_Path &
                                GNAT.Directory_Operations.Dir_Separator &
                                Base_Name;
               begin
                  --  copy ignoring '.' and '..' entries
                  if Base_Name /= "." and then Base_Name /= ".." then
                     if GNAT.OS_Lib.Is_Regular_File (Source) then
                        if GNAT.OS_Lib.Is_Regular_File (Target) then
                           Status := Posix_Shell.Rm.Delete_File (Target);
                        end if;

                        if Status /= 0 then
                           Error (S.all, "cp: cannot remove '" &
                                    Target & "': windows error " & Status'Img);
                           Got_Errors := Force;
                        end if;

                        Copy_File (Name     => Source,
                                   Pathname => Target,
                                   Success  => Success,
                                   Preserve => Preserve);

                        if not Success then
                           Error (S.all,
                                  "cp: '"  & Source & "' not copied to '" &
                                    Target & "'");
                           Got_Errors := True;
                        end if;

                     elsif GNAT.OS_Lib.Is_Directory (Source) then
                        Recursive_Copy (Source,
                                        Target);
                     end if;
                  end if;

               end;
            end loop;
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Got_Errors := True;
               Error (S.all,
                      "cp: unable to create such directory '" &
                        Target_Path & "'");
         end Recursive_Copy;

      begin

         if GNAT.OS_Lib.Is_Regular_File (Filename) then
            if not Target_Dir_Exists then

               if GNAT.OS_Lib.Is_Regular_File (Target_Name) then
                  Status := Posix_Shell.Rm.Delete_File (Target_Name);
               end if;

               if Status /= 0 then
                  Error (S.all, "cp: cannot remove '" &
                           Target_Name & "': windows error " & Status'Img);
                  Got_Errors := Force;
               end if;

            else
               declare
                  Target : constant String := Target_Name &
                         GNAT.Directory_Operations.Dir_Separator &
                         GNAT.Directory_Operations.Base_Name (Filename);
               begin
                  if GNAT.OS_Lib.Is_Regular_File (Target) then
                     Status :=
                       Posix_Shell.Rm.Delete_File (Target);
                  end if;

                  if Status /= 0 then
                     Error (S.all, "cp: cannot remove '" &
                              Target & "': windows error " & Status'Img);
                     Got_Errors := Force;
                  end if;

               end;
            end if;

            Copy_File (Name     => Filename,
                       Pathname => Target_Name,
                       Success  => Success,
                       Preserve => Preserve);

            if not Success then
               Error (S.all,
                      "cp: '"  & Filename & "' not copied to '" &
                        Target_Name & "'");
               Got_Errors := True;
            end if;

         elsif GNAT.OS_Lib.Is_Directory (Filename) then

            if not Recursive then
               Got_Errors := True;
               Error (S.all,
                      "cp: '" & Filename
                      &  "' is a directory (not copied)");
               return;
            end if;

            if not Target_Dir_Exists then
               Recursive_Copy (Filename,
                               Target_Name);
            else
               Recursive_Copy (Filename,
                               Target_Name &
                                 GNAT.Directory_Operations.Dir_Separator &
                                 GNAT.Directory_Operations.Base_Name (Filename)
                                 );
            end if;
         else
            --  File or directory to copy does not exist
            Got_Errors := True;
            Error (S.all,
                   "cp: no such file or directory '" & Filename & "' ");
         end if;
      end Cp_Tree;

   begin

      if Target_Name = "" then
         Error (S.all, "cp: missing operand");
         return 1;
      end if;

      --  Parse options
      for Index in Args'Range loop
         if Args (Index) (Args (Index)'First) = '-' then
            if Args (Index).all = "--" then
               File_List_Start := Index + 1;
               exit;
            elsif Args (Index).all = "-" then
               File_List_Start := Index;
               exit;
            end if;

            for C in Args (Index).all'First + 1 .. Args (Index).all'Last loop
               case Args (Index).all (C) is
                  when 'f' => Force := True;
                  when 'R' => Recursive := True;
                  when 'r' => Recursive := True;
                  when 'p' => Preserve := Full;
                  when others =>
                     Error (S.all, "cp: unknown option: " & Args (Index).all);
                     return 1;
               end case;
            end loop;
         else
            File_List_Start := Index;
            exit;
         end if;
      end loop;

      --  Check for operands presence.
      if File_List_Start > File_List_End then
         Error (S.all, "cp: missing operand");
         return 1;
      end if;

      Target_Dir_Exists := GNAT.OS_Lib.Is_Directory (Target_Name);

      if File_List_Start /= File_List_End then
         --  When a list of elements is to be copied, args'last must be an
         --  existing directory.
         if not Target_Dir_Exists then
            Error (S.all, "cp: no such directory '" & Target_Name & "'");
            return 1;
         end if;
      end if;

      --  Iterate the files
      for Index in File_List_Start .. File_List_End loop
         Cp_Tree (GNAT.OS_Lib.Normalize_Pathname
                  (Resolve_Path (S.all, Args (Index).all),
                     Resolve_Links => False));
      end loop;

      if Got_Errors then
         return 1;
      else
         return 0;
      end if;

   end Cp_Builtin;

end Posix_Shell.Builtins.Cp;
