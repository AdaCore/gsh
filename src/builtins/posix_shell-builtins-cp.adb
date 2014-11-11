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
   --  `-p` changes the preservation of copy from 'None' to 'Full'
   --      when using runtime 'Copy_File'

   function Cp_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      On_Windows      : constant Boolean := Directory_Separator = '\';
      Exec_Extension  : constant String := ".exe";
      File_List_Start : Integer := Args'First;
      File_List_End   : constant Integer := Args'Last - 1;
      Recursive       : Boolean   := False;
      Force           : Boolean   := False;
      Preserve        : Attribute := None;
      Got_Errors      : Boolean   := False;

      procedure Cp_Tree
        (Filename            : String;
         Target_Name         : String;
         Target_Is_Directory : Boolean);

      -------------
      -- Cp_Tree --
      -------------

      procedure Cp_Tree
        (Filename            : String;
         Target_Name         : String;
         Target_Is_Directory : Boolean)
      is

         Target_Path : constant String :=
           (if Target_Is_Directory then
               Target_Name &
               GNAT.Directory_Operations.Dir_Separator &
               GNAT.Directory_Operations.Base_Name (Filename)
            else
               Target_Name);

         procedure Recursive_Copy (Source_Path : String;
                                   Target_Path : String);

         procedure Simple_Copy (Source_Path : String;
                                Target_Path : String);

         -----------------
         -- Simple_Copy --
         -----------------

         procedure Simple_Copy (Source_Path : String;
                                Target_Path : String)
         is
            Status  : long := 0;
            Success : Boolean := False;
         begin
            if GNAT.OS_Lib.Is_Regular_File (Target_Path) then
               Status := Posix_Shell.Rm.Delete_File (Target_Path);
            end if;

            if Status /= 0 then
               Error (S, "cp: cannot remove '" &
                        Target_Path & "': windows error " & Status'Img);
               Got_Errors := Force;
            end if;

            Copy_File (Name     => Source_Path,
                       Pathname => Target_Path,
                       Success  => Success,
                       Preserve => Preserve);

            if not Success then
               Error (S,
                      "cp: '"  & Source_Path & "' not copied to '" &
                        Target_Path & "'");
               Got_Errors := True;
            end if;
         end Simple_Copy;

         --------------------
         -- Recursive_Copy --
         --------------------

         procedure Recursive_Copy (Source_Path : String;
                                   Target_Path : String) is

            Search  : Ada.Directories.Search_Type;
            Dir_Ent : Ada.Directories.Directory_Entry_Type;
         begin
            --  Check if target_path is a directory and it exists
            if GNAT.OS_Lib.Is_Regular_File (Target_Path) then
               Error  (S, "cp: " & Target_Path & "is not a directory");
               Got_Errors := True;
            elsif not GNAT.OS_Lib.Is_Directory (Target_Path) then
               GNAT.Directory_Operations.Make_Dir (Target_Path);
            end if;

            Ada.Directories.Start_Search (Search,
                                          Directory => Source_Path,
                                          Pattern   => "");

            while Ada.Directories.More_Entries (Search) loop
               Ada.Directories.Get_Next_Entry (Search, Dir_Ent);

               declare
                  Base_Name : constant String :=
                                Ada.Directories.Simple_Name (Dir_Ent);
                  Source    : constant String :=
                                Source_Path &
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
                        Simple_Copy (Source, Target);

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
               Error (S,
                      "cp: unable to create such directory '" &
                        Target_Path & "'");
         end Recursive_Copy;

      begin

         if GNAT.OS_Lib.Is_Directory (Filename) then

            if not Recursive then
               Got_Errors := True;
               Error (S,
                      "cp: '" & Filename
                      &  "' is a directory (not copied)");
               return;
            end if;
            Recursive_Copy (Filename, Target_Path);

         elsif GNAT.OS_Lib.Is_Regular_File (Filename) then
            Simple_Copy (Filename, Target_Path);

         elsif On_Windows and then
           GNAT.OS_Lib.Is_Regular_File (Filename & Exec_Extension)
         then
             --  Workaround for windows executables:
             --  In the following, when on a windows platform,
             --  If the source file does not exist then the source file with
             --  the exectension '.exe' is looked for.
             --  In this case, the target will also be extended by '.exe'
            Simple_Copy (Filename & Exec_Extension,
                         Target_Path & Exec_Extension);

         else
            --  File or directory to copy does not exist
            Got_Errors := True;
            Error (S,
                   "cp: no such file or directory '" & Filename & "' ");
         end if;
      end Cp_Tree;

   begin

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
                     Error (S, "cp: unknown option: " & Args (Index).all);
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
         Error (S, "cp: missing operand");
         return 1;
      end if;

      declare
         Target_Path : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (Resolve_Path (S,
                            Args (Args'Last).all),
              Resolve_Links => False);
         Target_Is_Directory : constant Boolean :=
           GNAT.OS_Lib.Is_Directory (Target_Path);
      begin

         if File_List_Start /= File_List_End then
            --  When a list of elements is to be copied, args'last must be an
            --  existing directory.
            if not Target_Is_Directory then
               Error (S, "cp: no such directory '" & Target_Path & "'");
               return 1;
            end if;
         end if;

         --  Iterate the files
         for Index in File_List_Start .. File_List_End loop
            Cp_Tree (GNAT.OS_Lib.Normalize_Pathname
                     (Resolve_Path (S, Args (Index).all),
                        Resolve_Links => False),
                     Target_Path,
                     Target_Is_Directory);
         end loop;

         if Got_Errors then
            return 1;
         else
            return 0;
         end if;
      end;
   end Cp_Builtin;

end Posix_Shell.Builtins.Cp;
