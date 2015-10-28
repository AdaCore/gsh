------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2015, AdaCore                   --
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

package body OS.Exec is

   procedure Normalize_Arguments
     (Args      : Argument_List;
      Norm_Args : in out Argument_List);

   function Portable_Execvp
     (Args   : System.Address;
      Cwd    : System.Address;
      Env    : System.Address;
      Stdin  : OS.FS.File_Descriptor;
      Stdout : OS.FS.File_Descriptor;
      Stderr : OS.FS.File_Descriptor)
      return Handle;
   pragma Import (C, Portable_Execvp, "__gsh_no_block_spawn");

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn
     (Args            : Argument_List;
      Cwd             : String                := "";
      Env             : Argument_List         := Null_Argument_List;
      Stdin_Fd        : OS.FS.File_Descriptor := OS.FS.Standin;
      Stderr_Fd       : OS.FS.File_Descriptor := OS.FS.Standerr;
      Status          : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use OS.FS;
      use Ada.Strings.Unbounded;

      Pipe_Input, Pipe_Output : OS.FS.File_Descriptor;
      Output                  : Unbounded_String;
      Real_Stderr_Fd          : OS.FS.File_Descriptor := Stderr_Fd;
      Buffer                  : String (1 .. 4096);
      Pid                     : Handle;
      N                       : Integer;
   begin
      Open_Pipe (Pipe_Input, Pipe_Output);

      if Stderr_Fd = To_Stdout then
         Real_Stderr_Fd := Pipe_Output;
      end if;

      Pid := Non_Blocking_Spawn
        (Args, Cwd, Env, Stdin_Fd, Pipe_Output, Real_Stderr_Fd);
      Close (Pipe_Output);
      loop
         N := Read (Pipe_Input, Buffer);
         if N > 0 then
            Output := Output & Buffer (1 .. N);
         end if;
         exit when N = 0;
      end loop;

      Status := Wait (Pid);
      return Output;
   end Blocking_Spawn;

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String                := "";
      Env       : Argument_List         := Null_Argument_List;
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr)
      return Integer
   is
      Pid    : Handle;
      Result : Integer;
   begin
      Pid := Non_Blocking_Spawn
        (Args, Cwd, Env, Stdin_Fd, Stdout_Fd, Stderr_Fd);
      Result := Wait (Pid);
      return Result;
   end Blocking_Spawn;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String                := "";
      Env       : Argument_List         := Null_Argument_List;
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr)
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

      use OS.FS;
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

      Set_Close_On_Exec (Stdin_Fd, False);
      Set_Close_On_Exec (Stdout_Fd, False);
      Set_Close_On_Exec (Stderr_Fd, False);

      Result := Portable_Execvp
        (C_Arg_List'Address, C_Cwd_Addr, C_Env_Addr,
         Stdin_Fd, Stdout_Fd, Stderr_Fd);
      for K in Arg_List'Range loop
         Free (Arg_List (K));
      end loop;

      Set_Close_On_Exec (Stdin_Fd, True);
      Set_Close_On_Exec (Stdout_Fd, True);
      Set_Close_On_Exec (Stderr_Fd, True);
      GNAT.Task_Lock.Unlock;
      return Result;
   end Non_Blocking_Spawn;

   -------------------------
   -- Normalize_Arguments --
   -------------------------

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

end OS.Exec;
