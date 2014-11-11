------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Rm                            --
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

with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations;

with Posix_Shell.Rm; use Posix_Shell.Rm;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Rm is

   ----------------
   -- Rm_builtin --
   ----------------

   function Rm_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is

      File_List_Start : Integer := Args'First;
      Recursive       : Boolean := False;
      Force           : Boolean := False;
      Got_Errors      : Boolean := False;

      procedure Rm_Tree (Filename : String);

      procedure Rm_Tree (Filename : String) is
         Search      : Search_Type;
         Dir_Ent     : Directory_Entry_Type;
         Status      : long;
      begin
         if GNAT.OS_Lib.Is_Regular_File (Filename) then
            Status := Delete_File (Filename);
            if Status /= 0 then
               Error (S, "rm: cannot remove `" &
                        Filename & "': windows error " & Status'Img);
               if not Force then
                  Got_Errors := True;
               end if;
            end if;
         elsif GNAT.OS_Lib.Is_Directory (Filename) then

            if not Recursive then
               Error (S, "rm: cannot remove `" &
                        Filename & "': is a directory");
               Got_Errors := True;
               return;
            end if;

            Start_Search (Search, Directory => Filename, Pattern => "");
            while More_Entries (Search) loop
               Get_Next_Entry (Search, Dir_Ent);

               declare
                  Base_Name : constant String :=
                    Simple_Name (Dir_Ent);
                  File_Name : constant String :=
                    Filename & GNAT.Directory_Operations.Dir_Separator &
                    Base_Name;
               begin
                  if GNAT.OS_Lib.Is_Directory (File_Name) then
                     --  Recurse ignoring '.' and '..' entries
                     if Base_Name /= "." and then Base_Name /= ".." then
                        Rm_Tree (File_Name);
                     end if;
                  else
                     Status := Delete_File (File_Name);
                     if Status /= 0 then
                        Error (S, "rm: cannot remove `" &
                                 File_Name & "': windows error " & Status'Img);
                        if not Force then
                           Got_Errors := True;
                        end if;
                     end if;
                  end if;
               end;

            end loop;
            End_Search (Search);

            Status := Delete_File (Filename);
            if Status /= 0 then
               Error (S, "rm: cannot remove `" &
                        Filename & "': windows error " & Status'Img);
               if not Force then
                  Got_Errors := True;
               end if;
            end if;
         elsif not Force then
            --  File does not exist emit warning if necessary
            Got_Errors := True;
            Error (S, "rm: cannot remove `" & Filename &
                     "': no such file or directory");
         end if;
      end Rm_Tree;

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
                  when 'i' => Force := False;
                  when 'R' => Recursive := True;
                  when 'r' => Recursive := True;
                  when others =>
                     Error (S, "rm: unknown option: " & Args (Index).all);
                     return 1;
               end case;
            end loop;
         else
            File_List_Start := Index;
            exit;
         end if;
      end loop;

      --  Check for operands presence.
      if File_List_Start > Args'Last then
         if not Force then
            Error (S, "rm: missing operand");
            return 1;
         else
            return 0;
         end if;
      end if;

      --  Iterate other the files
      for Index in File_List_Start .. Args'Last loop
         Rm_Tree (GNAT.OS_Lib.Normalize_Pathname
                  (Resolve_Path (S, Args (Index).all),
                     Resolve_Links => False));
      end loop;
      if Got_Errors then
         return 1;
      else
         return 0;
      end if;
   end Rm_Builtin;

end Posix_Shell.Builtins.Rm;
