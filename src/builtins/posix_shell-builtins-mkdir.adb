------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Mkdir                         --
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

with GNAT.Directory_Operations;

with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Builtins.Support; use Posix_Shell.Builtins.Support;

package body Posix_Shell.Builtins.Mkdir is

   -------------------
   -- Mkdir_Builtin --
   -------------------

   function Mkdir_Builtin
     (S    : in out Shell_State;
      Args : String_List)
      return Eval_Result
   is
      File_List_Start : Integer := Args'First;
      Create_Intermediates : Boolean := False;
      Got_Errors : Boolean := False;

   begin
         --  Parse options
      for Index in Args'Range loop
         if Args (Index) (Args (Index)'First) = '-' then
            if Args (Index).all = "--" then
               File_List_Start := Index + 1;
               exit;
            end if;

            for C in Args (Index).all'First + 1 .. Args (Index).all'Last loop
               case Args (Index).all (C) is
                  when 'p' => Create_Intermediates := True;
                  when others =>
                     Error (S, "mkdir: unknown option: " &
                            Args (Index).all);
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
         Error (S, "mkdir: too few arguments");
         return (RESULT_STD, 1);
      end if;

      --  Iterate other the files
      for Index in File_List_Start .. Args'Last loop
         declare
            CP : constant String := Normalize_Path (S, Args (Index).all);
         begin
            if Create_Intermediates then
               Recursive_Make_Dir (CP);
            else
               GNAT.Directory_Operations.Make_Dir (CP);
            end if;
         exception
            when others =>
               Put (S, 2, "cannot create " & CP & ASCII.LF);
               Got_Errors := True;
         end;
      end loop;

      if Got_Errors then
         return (RESULT_STD, 1);
      else
         return (RESULT_STD, 0);
      end if;
   end Mkdir_Builtin;

end Posix_Shell.Builtins.Mkdir;
