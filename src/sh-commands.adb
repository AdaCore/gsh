------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

with Sh.Builtins; use Sh.Builtins;
with Sh.States.IO; use Sh.States.IO;
with Sh.Functions; use Sh.Functions;
with Sh.Utils; use Sh.Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Text_IO;
with OS.Exec; use OS.Exec;

package body Sh.Commands is

   function Get_Launcher
     (S : Shell_State; Filename : String_Access) return String_List;

   function Get_Launcher
     (S : Shell_State; Filename : String_Access)
      return String_List
   is
      Fd           : File_Descriptor;
      Buffer       : aliased String (1 .. 256);
      Buffer_Last  : Natural := 0;
      Result       : String_List (1 .. 32);
      Result_Last  : Natural := 0;
   begin
      Fd := Open_Read (Filename.all, Binary);
      --  Here we read only 256 characters. That should be sufficient
      Buffer_Last := Read (Fd, Buffer'Address, 256);
      Close (Fd);

      if Buffer_Last >= 2 and then Buffer (1 .. 2) = "#!" then
         --  If the first two character of the file to execute are #! then
         --  this is a script. Find which program should be used to launch it
         declare
            Token_First : Natural := 0;
         begin

            for J in 3 .. Buffer_Last loop
               --  First check for token delimiters
               if Token_First > 0
                 and then (Buffer (J) = ' ' or else Buffer (J) = ASCII.LF
                           or else Buffer (J) = ASCII.CR)
               then
                  Result_Last := Result_Last + 1;
                  if Result_Last = 1 then
                     --  Don't trust paths in '#!". Usually they are unix paths
                     --  just ignore it for the time being
                     Result (Result_Last) :=
                       new String'(Base_Name (Buffer (Token_First .. J - 1)));
                  else
                     Result (Result_Last) :=
                       new String'(Buffer (Token_First .. J - 1));
                  end if;
                  Token_First := 0;
               end if;

               case Buffer (J) is
                  when ' '      => null; -- skip spaces
                  when ASCII.CR => exit; -- a CR mark the end of the parsing
                  when ASCII.LF => exit; -- a LF mark the end of the parsing
                  when others =>
                     if Token_First = 0 then
                        Token_First := J;
                     end if;
               end case;
            end loop;

         end;
      end if;

      Result_Last := Result_Last + 1;
      Result (Result_Last) := Filename;

      --  Check if the command to spawn is either bash or sh. Replace by gsh
      --  call.
      declare
         Original_Cmd : constant String := Filename.all;
         Cmd : constant String := Base_Name (Result (Result'First).all,
                                             ".exe");
      begin
         if Cmd = "bash" or Cmd = "sh" then
            Free (Result (1));
            Result (1) := new String'("gsh");
         end if;

         if Result (1).all /= Original_Cmd then
            --  resolve now the launcher path
            declare
               Tmp : constant String_Access := Locate_Exec (S, Result (1).all);
            begin
               Free (Result (1));
               Result (1) := Tmp;
            end;

         end if;
      end;

      return Result (1 .. Result_Last);

   end Get_Launcher;

   ---------
   -- Run --
   ---------

   function Run
     (S    : in out Shell_State;
      Cmd  : String;
      Args : String_List;
      Env  : String_List)
      return Eval_Result
   is
      Exec_Path   : String_Access := null;
      Exit_Status : Eval_Result;
      Args_First  : Integer := Args'First;
      Opt_Args    : access String_List := null;
   begin

      --  Output command line if -x is set
      if Is_Xtrace_Enabled (S) then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "+ " & Cmd);
         for I in Args'Range loop
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             " '" & Args (I).all & "'");
         end loop;
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end if;

      --  Handle builtins first.
      if Is_Builtin (Cmd) then
         Exit_Status := Execute_Builtin (S, Cmd, Args);
         if Exit_Status.Kind = RESULT_STD then
            Save_Last_Exit_Status (S, Exit_Status.Status);
         end if;
         --  Ada.Text_IO.Put_Line ("from builtin " & Exit_Status.Kind'Img);
         return Exit_Status;
      end if;

      --  Next, is this a function ?
      if Is_Function (S, Cmd) then
         return Execute_Function (S, Cmd, Args);
      end if;

      --  This command can only be an executable. See if we can
      --  locate an executable using the various possible filename
      --  extensions. Note that if we are trying to spawn cmd we should not try
      --  to compute the full path otherwise it will result in a strange
      --  message: "the syntax of the command is incorrect".
      if Cmd = "cmd" or else Cmd = "cmd.exe" then
         Exec_Path := new String'("cmd.exe");

         --  This hooks is used in to fix an issue with script written for
         --  mingw environment. Indeed in mingw cmd in non interactive mode
         --  is launched using //c rather than /c. Ensure that if first
         --  argument to cmd is //c then it is replaced by /c
         if Args'Length > 0 and then Args (Args'First).all = "//c" then
            Opt_Args := new String_List'(1 => new String'("/c"));
            Args_First := Args'First + 1;
         end if;

      else
         Exec_Path := Locate_Exec (S, Cmd);
      end if;

      if Exec_Path = null then
         Put (S, 2, Cmd & ": command not found"); New_Line (S, 2);
         return (RESULT_STD, 127);
      end if;

      if Is_Xtrace_Enabled (S) then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                          "+ resolve to " & Exec_Path.all);
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end if;

      declare
         Launcher : String_List := Get_Launcher (S, Exec_Path);
         Cmd_Line : constant String_List :=
           (if Opt_Args /= null then
               Launcher & Opt_Args.all & Args (Args_First .. Args'Last) else
                 Launcher & Args (Args_First .. Args'Last));
      begin

         if Cmd_Line (Cmd_Line'First) = null then
            Put (S, 2, Cmd & ": can't launch program");
            New_Line (S, 2);
            Exit_Status := (RESULT_STD, 127);
         else

            if Is_Xtrace_Enabled (S) then
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                                "+ resolve to " &
                                  Cmd_Line (Cmd_Line'First).all);
               Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            end if;
            Exit_Status := (RESULT_STD, Blocking_Spawn
              (Cmd_Line,
               Get_Current_Dir (S),
               Env,
               Get_File_Descriptor (S, 0),
               Get_File_Descriptor (S, 1),
               Get_File_Descriptor (S, 2)));
         end if;
         for J in Launcher'Range loop
            Free (Launcher (J));
         end loop;

      exception
         when Program_Error =>
            for J in Launcher'Range loop
               Free (Launcher (J));
            end loop;
            Exit_Status := (RESULT_STD, 127);
      end;

      if Opt_Args /= null then
         for J in Opt_Args'Range loop
            Free (Opt_Args (J));
         end loop;
         Free (Opt_Args);
      end if;

      return Exit_Status;
   end Run;

end Sh.Commands;
