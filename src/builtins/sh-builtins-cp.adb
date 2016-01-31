------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Cp                            --
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

with Interfaces.C;                 use Interfaces.C;
with GNAT.Directory_Operations;

with Sh.Rm;               use Sh.Rm;
with Sh.States.Output; use Sh.States.Output;
with OS.FS;                        use OS.FS;
with OS.FS.Stat;                   use OS.FS.Stat;
with OS.FS.Dir;

package body Sh.Builtins.Cp is

   ----------------
   -- Cp_builtin --
   ----------------

   --  treatment of options:
   --  `-f` is not actually used. Files are always removed if possible.
   --      users rights are not taken into account.
   --  `-p` changes the preservation of copy from 'None' to 'Full'
   --      when using runtime 'Copy_File'

   function Cp_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result
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
         Target_Attrs        : File_Attributes);

      -------------
      -- Cp_Tree --
      -------------

      procedure Cp_Tree
        (Filename     : String;
         Target_Name  : String;
         Target_Attrs : File_Attributes)
      is

         Target_Path : constant String :=
           (if Is_Directory (Target_Attrs) then
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
            Success : unsigned_long;
         begin
            if GNAT.OS_Lib.Is_Regular_File (Target_Path) then
               Status := Sh.Rm.Delete_File (Target_Path);
            end if;

            if Status /= 0 then
               Error (S, "cp: cannot remove '" &
                        Target_Path & "': windows error " & Status'Img);
               if Force then
                  Got_Errors := True;
               end if;
            end if;

            Success := Copy_File (Source   => Source_Path,
                                  Target   => Target_Path,
                                  Fail_If_Exists => False,
                                  Preserve_Attributes => Preserve /= None);
            if Success /= 0 then
               --  Retry a second time as we sometimes get some unknown
               --  failures.
               delay 0.1;
               Error (S, "cp: '" & Source_Path  & "' to '" &
                        Target_Path & "' failed on first attempt" &
                        Success'Img);
               Success := Copy_File (Source     => Source_Path,
                                     Target     => Target_Path,
                                     Fail_If_Exists => False,
                                     Preserve_Attributes => Preserve /= None);
               if Success /= 0 and Preserve = Full then
                  --  In case of two failures in a row and Preserve is set to
                  --  True, try one last time without preserving attributes.
                  Error (S, "cp: '" & Source_Path & "' to '" &
                           Target_Path & "' discard permission preserve" &
                           Success'Img);
                  Success := Copy_File
                    (Source              => Source_Path,
                     Target              => Target_Path,
                     Fail_If_Exists       => False,
                     Preserve_Attributes => Preserve /= None);
               end if;
            end if;

            if Success /= 0 then
               Error (S,
                      "cp: '"  & Source_Path & "' not copied to '" &
                        Target_Path & "'" & Success'Img);
               Got_Errors := True;
            end if;
         end Simple_Copy;

         --------------------
         -- Recursive_Copy --
         --------------------

         procedure Recursive_Copy (Source_Path : String;
                                   Target_Path : String) is

            Search  : OS.FS.Dir.Dir_Handle;
            Dir_Ent : OS.FS.Dir.Dir_Entry;

            Target_Path_Attrs : constant File_Attributes :=
              File_Information (Target_Path);
         begin
            --  Check if target_path is a directory and it exists
            if Is_Regular_File (Target_Path_Attrs) then
               Error  (S, "cp: " & Target_Path & "is not a directory");
               Got_Errors := True;
            elsif not Is_Directory (Target_Path_Attrs) then
               GNAT.Directory_Operations.Make_Dir (Target_Path);
            end if;

            Search := OS.FS.Dir.Open (Source_Path);

            loop
               Dir_Ent := OS.FS.Dir.Read (Search);
               exit when OS.FS.Dir.Is_Null (Dir_Ent);

               declare
                  Base_Name : constant String := OS.FS.Dir.Name (Dir_Ent);
                  Source    : constant String :=
                    Source_Path &
                    GNAT.Directory_Operations.Dir_Separator &
                    Base_Name;
                  Target    : constant String :=
                    Target_Path &
                    GNAT.Directory_Operations.Dir_Separator &
                    Base_Name;
                  Source_Attrs : constant File_Attributes :=
                    OS.FS.Dir.File_Information (Dir_Ent);
               begin
                  if Is_Regular_File (Source_Attrs) then
                     Simple_Copy (Source, Target);
                  elsif Is_Directory (Source_Attrs) then
                     Recursive_Copy (Source, Target);
                  end if;
               end;
            end loop;

            OS.FS.Dir.Close (Search);
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Got_Errors := True;
               Error (S,
                      "cp: unable to create such directory '" &
                        Target_Path & "'");
         end Recursive_Copy;

         Filename_Attrs : constant File_Attributes :=
           File_Information (Filename);
      begin
         if Is_Directory (Filename_Attrs) then

            if not Recursive then
               Got_Errors := True;
               Error (S,
                      "cp: '" & Filename
                      &  "' is a directory (not copied)");
               return;
            end if;
            Recursive_Copy (Filename, Target_Path);

         elsif Is_Regular_File (Filename_Attrs) then
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
                     return (RESULT_STD, 1);
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
         return (RESULT_STD, 1);
      end if;

      declare
         Target_Path : constant String :=
           Normalize_Path (S, Args (Args'Last).all);
         Target_Attrs : constant File_Attributes :=
           File_Information (Target_Path);
      begin
         if File_List_Start /= File_List_End then
            --  When a list of elements is to be copied, args'last must be an
            --  existing directory.
            if not Is_Directory (Target_Attrs) then
               Error (S, "cp: no such directory '" & Target_Path & "'");
               return (RESULT_STD, 1);
            end if;
         end if;

         --  Iterate the files
         for Index in File_List_Start .. File_List_End loop
            Cp_Tree (Normalize_Path (S, Args (Index).all),
                     Target_Path,
                     Target_Attrs);
         end loop;

         if Got_Errors then
            return (RESULT_STD, 1);
         else
            return (RESULT_STD, 0);
         end if;
      end;
   end Cp_Builtin;

end Sh.Builtins.Cp;
