------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                              Sh.Builtins.Mv                              --
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

with OS.FS; use OS.FS;
with OS.FS.Stat; use OS.FS.Stat;
with Sh.States.IO; use Sh.States.IO;

with GNAT.Directory_Operations;

package body Sh.Builtins.Mv is

   function Mv_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result
   is
      Status             : Integer;
      Source_Index       : Integer := Args'First;
      Overwrite_If_Exist : Boolean := False;
      Has_Errors         : Boolean := False;
   begin
      --  Handle options
      loop
         exit when Source_Index > Args'Last;
         declare
            Arg : constant String := Args (Source_Index).all;
         begin

            if Arg (Arg'First) = '-' then
               if Arg = "--" then
                  Source_Index := Source_Index + 1;
                  exit;

               elsif Arg = "-f" then
                  Overwrite_If_Exist := True;
               else
                  Error (S, "mv: invalid option " & Arg);
                  return (RESULT_STD, 1);
               end if;
            else
               exit;
            end if;
         end;
         Source_Index := Source_Index + 1;
      end loop;

      --  We need at least two positional arguments
      if Source_Index > Args'Last - 1 then
         Error (S, "mv: need at least two arguments: source and destination");
         return (RESULT_STD, 1);
      end if;

      declare
         Target : constant String := Normalize_Path (S, Args (Args'Last).all);
         Is_Target_Dir : constant Boolean := Is_Directory
           (File_Information (Target));
      begin

         if Source_Index /= Args'Last - 1 and then not Is_Target_Dir then
            --  We have multiple sources so ensure that target is an existing
            --  directory.
            Error (S, "mv: " & Target & " is not a directory");
            return (RESULT_STD, 1);
         end if;

         --  Move all sources to target
         for Index in Source_Index .. Args'Last - 1 loop
            declare
               Source : constant String := Normalize_Path
                 (S, Args (Index).all);
               Final_Target : constant String :=
                 (if Is_Target_Dir then Join
                    (Target, GNAT.Directory_Operations.Base_Name (Source))
                  else Target);
            begin

               Status := Move_File
                 (Source, Final_Target, Overwrite_If_Exist);
               --  On error do not abort but display an error and ensure
               --  that final status will be an error
               if Status /= 0 then
                  Error (S,
                         "mv: cannot move " & Source & " to " & Target);
                  Has_Errors := True;
               end if;
            end;
         end loop;

         --  Compute final status
         if Has_Errors then
            return (RESULT_STD, 1);
         else
            return (RESULT_STD, 0);
         end if;
      end;
   end Mv_Builtin;

end Sh.Builtins.Mv;
