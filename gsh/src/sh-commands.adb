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
with Sh.Traces;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Text_IO;
with OS.Exec; use OS.Exec;

package body Sh.Commands is

   procedure Add_Launcher
     (State    : Shell_State;
      Cmd_Line : in out CList;
      Filename : String);

   procedure Add_Launcher
     (State    : Shell_State;
      Cmd_Line : in out CList;
      Filename : String)
   is
      Fd           : File_Descriptor;
      Buffer       : aliased String (1 .. 256);
      Buffer_Last  : Integer := 0;
      --  Result       : String_List (1 .. 32);
      --  Result_Last  : Natural := 0;
   begin
      Fd := Open_Read (Filename, Binary);
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
                  if Length (Cmd_Line) = 0 then
                     --  Don't trust paths in '#!". Usually they are unix paths
                     --  just ignore it for the time being
                     declare
                        Cmd : constant String :=
                          Base_Name (Buffer (Token_First .. J - 1));
                        Cmd_Base : constant String :=
                          Base_Name (Buffer (Token_First .. J - 1), ".exe");
                     begin
                        if Cmd_Base = "bash" or else Cmd_Base = "sh" then
                           Append (Cmd_Line, Locate_Exec (State, "gsh").all);
                        else
                           Append (Cmd_Line, Locate_Exec (State, Cmd).all);
                        end if;
                     end;
                  else
                     Append (Cmd_Line, Buffer (Token_First .. J - 1));
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

      Append (Cmd_Line, Filename);
   end Add_Launcher;

   ---------
   -- Run --
   ---------

   function Run
     (S    : in out Shell_State;
      Cmd  : String;
      Args : CList;
      Env  : String_List)
      return Eval_Result
   is
      Exec_Path   : String_Access := null;
      Exit_Status : Eval_Result := (RESULT_STD, 0);
      First_Arg   : Natural := 2;
      Cmd_Line    : CList;
   begin

      --  Output command line if -x is set
      if Is_Xtrace_Enabled (S) then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "+ ");
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                          Join (Args, " ", "'"));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end if;

      --  Handle builtins first.
      if Is_Builtin (Cmd) then
         Sh.Traces.Log (Sh.Traces.LOG_EXEC, As_List (Args));
         Exit_Status := Execute_Builtin (S, Cmd, Args);
         if Exit_Status.Kind = RESULT_STD then
            Save_Last_Exit_Status (S, Exit_Status.Status);
         end if;
      elsif Is_Function (S, Cmd) then
         Exit_Status := Execute_Function (S, Cmd, Args);
      else

         --  This command can only be an executable. See if we can
         --  locate an executable using the various possible filename
         --  extensions. Note that if we are trying to spawn cmd we should not
         --  try to compute the full path otherwise it will result in a strange
         --  message: "the syntax of the command is incorrect".
         if Cmd = "cmd" or else Cmd = "cmd.exe" then
            Append (Cmd_Line, "cmd.exe");

            --  This hooks is used in to fix an issue with script written for
            --  mingw environment. Indeed in mingw cmd in non interactive mode
            --  is launched using //c rather than /c. Ensure that if first
            --  argument to cmd is //c then it is replaced by /c
            if Length (Args) >= First_Arg
               and then Element (Args, First_Arg) = "//c"
            then
               Append (Cmd_Line, "/c");
               First_Arg := First_Arg + 1;
            end if;
         else
            Exec_Path := Locate_Exec (S, Cmd);
            if Exec_Path = null then
               Put (S, 2, Cmd & ": command not found"); New_Line (S, 2);
               Exit_Status := (RESULT_STD, 127);
            else
               Add_Launcher (S, Cmd_Line, Exec_Path.all);
            end if;
         end if;

         if Exit_Status /= (RESULT_STD, 127) then
            for Idx in First_Arg .. Length (Args) loop
               Append (Cmd_Line, Element (Args, Idx));
            end loop;

            declare
               Cmd_Line2 : constant String_List := As_List (Cmd_Line);
            begin

               Sh.Traces.Log (Sh.Traces.LOG_EXEC, Cmd_Line2);
               Exit_Status := (RESULT_STD, Blocking_Spawn
                               (Cmd_Line2,
                                  Get_Current_Dir (S),
                                  Env,
                                  Get_File_Descriptor (S, 0),
                                  Get_File_Descriptor (S, 1),
                                  Get_File_Descriptor (S, 2)));
            exception
               when Program_Error =>
                  Exit_Status := (RESULT_STD, 127);
            end;
         end if;
      end if;
      Deallocate (Cmd_Line);
      return Exit_Status;
   end Run;

end Sh.Commands;
