------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Rm                            --
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

with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations;

with Posix_Shell.Rm; use Posix_Shell.Rm;
with Posix_Shell.States.Output; use Posix_Shell.States.Output;
with OS.FS.Dir;
with OS.FS.Stat;

package body Posix_Shell.Builtins.Rm is

   ----------------
   -- Rm_builtin --
   ----------------

   function Rm_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result
   is

      File_List_Start : Integer := Args'First;
      Recursive       : Boolean := False;
      Force           : Boolean := False;
      Got_Errors      : Boolean := False;

      procedure Rm_Tree (Filename : String;
                         Filename_Info : OS.FS.Stat.File_Attributes);

      procedure Rm_Tree (Filename : String;
                         Filename_Info : OS.FS.Stat.File_Attributes)
      is
         Search      : OS.FS.Dir.Dir_Handle;
         Dir_Ent     : OS.FS.Dir.Dir_Entry;

         Status      : long;
      begin
         if OS.FS.Stat.Is_Regular_File (Filename_Info) then
            Status := Delete_File (Filename);
            if Status /= 0 then
               Error (S, "rm: cannot remove `" &
                        Filename & "': windows error " & Status'Img);
               if not Force then
                  Got_Errors := True;
               end if;
            end if;
         elsif OS.FS.Stat.Is_Directory (Filename_Info) then

            if not Recursive then
               Error (S, "rm: cannot remove `" &
                        Filename & "': is a directory");
               Got_Errors := True;
               return;
            end if;

            Search := OS.FS.Dir.Open (Filename);
            loop
               Dir_Ent := OS.FS.Dir.Read (Search);
               exit when OS.FS.Dir.Is_Null (Dir_Ent);

               declare
                  Base_Name : constant String := OS.FS.Dir.Name (Dir_Ent);
                  File_Name : constant String :=
                    Filename & GNAT.Directory_Operations.Dir_Separator &
                    Base_Name;
                  FI        : constant OS.FS.Stat.File_Attributes :=
                    OS.FS.Dir.File_Information (Dir_Ent);
               begin

                  if OS.FS.Stat.Is_Directory (FI) then
                     --  Recurse ignoring '.' and '..' entries
                     Rm_Tree (File_Name, FI);
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
            OS.FS.Dir.Close (Search);

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
                     return (RESULT_STD, 1);
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
            return (RESULT_STD, 1);
         else
            return (RESULT_STD, 0);
         end if;
      end if;

      --  Iterate other the files
      for Index in File_List_Start .. Args'Last loop
         declare
            Filename : constant String :=
              Normalize_Path (S, Args (Index).all);
            Filename_Info : constant OS.FS.Stat.File_Attributes :=
              OS.FS.Stat.File_Information (Filename);
         begin
            Rm_Tree (Filename, Filename_Info);
         end;
      end loop;
      if Got_Errors then
         return (RESULT_STD, 1);
      else
         return (RESULT_STD, 0);
      end if;
   end Rm_Builtin;

end Posix_Shell.Builtins.Rm;
