------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

with System;
with GNAT.Task_Lock;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Exec is

   procedure Normalize_Arguments
     (Args : Argument_List;
      Norm_Args : in out Argument_List);

   function Waitpid (H : Handle) return Integer;
   pragma Import (C, Waitpid, "__gsh_waitpid");
   --  Wait for a specific process id, and return its exit code

   function Portable_Execvp
     (Args : System.Address;
      Cwd  : System.Address;
      Env  : System.Address)
      return Handle;
   pragma Import (C, Portable_Execvp, "__gsh_no_block_spawn");

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String;
      Env       : Argument_List;
      Stdin_Fd  : File_Descriptor;
      Stdout_Fd : File_Descriptor;
      Stderr_Fd : File_Descriptor)
      return Integer
   is
      Pid    : Handle;
      Result : Integer;
   begin
      Pid := Non_Blocking_Spawn
        (Args, Cwd, Env, Stdin_Fd, Stdout_Fd, Stderr_Fd);
      Result := Waitpid (Pid);
      return Result;
   end Blocking_Spawn;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String;
      Env       : Argument_List;
      Stdin_Fd  : File_Descriptor;
      Stdout_Fd : File_Descriptor;
      Stderr_Fd : File_Descriptor)
      return Handle
   is
      Result     : Handle;
      Arg_List   : String_List (1 .. Args'Length + 1) := (others => null);
      C_Arg_List : aliased array (1 .. Args'Length + 1) of System.Address;
      C_Cwd      : aliased String (1 .. Cwd'Length + 1);
      C_Cwd_Addr : System.Address := System.Null_Address;
      C_Env      : aliased array (1 .. Env'Length + 1) of System.Address;
      C_Env_Addr : System.Address := System.Null_Address;
      Success : Boolean;
      pragma Warnings (Off, Success);

   begin
      GNAT.Task_Lock.Lock;
      Normalize_Arguments (Args, Arg_List);
      --  Prepare an array of arguments to pass to C

      Arg_List (Arg_List'Last) := null;

      --  Prepare low-level argument list from the normalized arguments

      for K in Arg_List'Range loop
         if Arg_List (K) /= null then
            C_Arg_List (K) := Arg_List (K).all'Address;
         else
            C_Arg_List (K) := System.Null_Address;
         end if;
      end loop;

      if Cwd /= "" then
         C_Cwd (Cwd'Range) := Cwd;
         C_Cwd (C_Cwd'Last) := ASCII.NUL;
         C_Cwd_Addr := C_Cwd'Address;
      end if;

      if Env'Length > 0 then
         for K in Env'Range loop
            C_Env (K - Env'First + C_Env'First) := Env (K).all'Address;
         end loop;
         C_Env (C_Env'Last) := System.Null_Address;
         C_Env_Addr := C_Env'Address;
      end if;

      --  This does not return on Unix systems
      declare
         Input, Output, Error : File_Descriptor;

      begin
         --  Since Windows does not have a separate fork/exec, we need to
         --  perform the following actions:
         --    - save stdin, stdout, stderr
         --    - replace them by our pipes
         --    - create the child with process handle inheritance
         --    - revert to the previous stdin, stdout and stderr.

         if Stdin_Fd /= 0 then
            Input  := Dup (Standin);
            Dup2 (Stdin_Fd,  GNAT.OS_Lib.Standin);
         end if;

         if Stdout_Fd /= 1 then
            Output := Dup (Standout);
            --  Ada.Text_IO.Put_Line (Stdout_Fd'Img & ", " & Output'Img);
            Dup2 (Stdout_Fd, GNAT.OS_Lib.Standout);

         end if;

         if Stderr_Fd /= 2 then
            Error  := Dup (Standerr);

            --  Since we are still called from the parent process, there is
            --  no way currently we can cleanly close the unneeded ends of
            --  the pipes, but this doesn't really matter. We could close
            --  Pipe1.Output, Pipe2.Input, Pipe3.Input.

            Dup2 (Stderr_Fd, GNAT.OS_Lib.Standerr);
         end if;

         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standin, False, Success);
         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standout, False, Success);
         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standerr, False, Success);

         Result := Portable_Execvp
           (C_Arg_List'Address, C_Cwd_Addr, C_Env_Addr);
         for K in Arg_List'Range loop
            Free (Arg_List (K));
         end loop;

         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standin, True, Success);
         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standout, True, Success);
         Posix_Shell.Variables.Output.Set_Close_On_Exec
           (GNAT.OS_Lib.Standerr, True, Success);
         --  Restore the old descriptors

         if Stdin_Fd /= 0 then
            Dup2 (Input,  GNAT.OS_Lib.Standin);
            Close (Input);
         end if;

         if Stdout_Fd /= 1 then
            Dup2 (Output, GNAT.OS_Lib.Standout);
            Close (Output);
         end if;

         if Stderr_Fd /= 2 then
            Dup2 (Error,  GNAT.OS_Lib.Standerr);
            Close (Error);
         end if;
      end;
      GNAT.Task_Lock.Unlock;
      return Result;
   end Non_Blocking_Spawn;

   procedure Normalize_Arguments
     (Args : Argument_List;
      Norm_Args : in out Argument_List)
   is

      procedure Quote_Argument
        (Arg : String;  Norm_Arg : in out String_Access);
      --  Add quote around argument if it contains spaces

      C_Argument_Needs_Quote : Integer;
      pragma Import (C, C_Argument_Needs_Quote, "__gnat_argument_needs_quote");
      Argument_Needs_Quote : constant Boolean := C_Argument_Needs_Quote /= 0;

      --------------------
      -- Quote_Argument --
      --------------------

      procedure Quote_Argument
        (Arg : String;  Norm_Arg : in out String_Access)
      is
         Res          : String (1 .. Arg'Length * 2 + 3);
         J            : Positive := 1;
         Quote_Needed : Boolean  := False;

      begin
         --  Starting quote

         Res (J) := '"';

         for K in Arg'Range loop

            J := J + 1;

            --  If a quote is found escape it with backslash

            if Arg (K) = '"' then

               --  If the quote is preceded by a backslash escape also the
               --  backslash

               --  Double all previous backslashes
               declare
                  Index  : Integer := K - 1;
               begin
                  while Index >= Arg'First and then Arg (Index) = '\' loop
                     Res (J) := '\';
                     J := J + 1;
                     Index := Index - 1;
                  end loop;
                  --  if K > Arg'First and then Arg (K - 1) = '\' then
                  --   Res (J) := '\';
                  --    J := J + 1;
                  --  end if;
               end;

               Res (J) := '\';
               J := J + 1;
               Res (J) := '"';

            elsif Arg (K) = ' ' or else Arg (K) = ASCII.HT then
               Res (J) := Arg (K);
               Quote_Needed := True;
            else
               Res (J) := Arg (K);
            end if;

         end loop;

         J := J + 1;
         Res (J) := '"';

         if Quote_Needed or else Arg'Length = 0 then
            Norm_Arg := new String'(Res (1 .. J) & ASCII.NUL);
         else
            Norm_Arg := new String'(Res (2 .. J - 1) & ASCII.NUL);
         end if;
      end Quote_Argument;

      --  Start of processing for Normalize_Arguments

   begin
      if Argument_Needs_Quote then
         for K in Args'Range loop
            if Args (K) /= null then
               Quote_Argument
                 (Args (K).all, Norm_Args (K - Args'First + Norm_Args'First));
            end if;
         end loop;
      else
         for K in Args'Range loop
            if Args (K) /= null then
               Norm_Args (K - Args'First + Norm_Args'First) :=
                 new String'(Args (K).all & ASCII.NUL);
            end if;
         end loop;
      end if;
   end Normalize_Arguments;

   ----------------
   -- Shell_Exit --
   ----------------

   procedure Shell_Exit (S : in out Shell_State; Code : Integer) is
   begin
      Save_Last_Exit_Status (S, Code);
      raise Shell_Exit_Exception;
   end Shell_Exit;

end Posix_Shell.Exec;
