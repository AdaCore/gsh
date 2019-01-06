------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2019, AdaCore                   --
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
with GNAT.Strings; use GNAT.Strings;
with GNAT.Task_Lock;

package body OS.Exec is

   procedure Normalize_Arguments
     (Args      : CList;
      Norm_Args : in out CList);

   function Portable_Execvp
     (Args     : System.Address;
      Cwd      : System.Address;
      Env      : System.Address;
      Stdin    : OS.FS.File_Descriptor;
      Stdout   : OS.FS.File_Descriptor;
      Stderr   : OS.FS.File_Descriptor;
      Priority : Priority_Class)
      return Handle;
   pragma Import (C, Portable_Execvp, "__gsh_no_block_spawn");

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn
     (Args            : CList;
      Env             : CList;
      Cwd             : String                := "";
      Stdin_Fd        : OS.FS.File_Descriptor := OS.FS.Standin;
      Stderr_Fd       : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority        : Priority_Class        := INHERIT;
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
        (Args, Env, Cwd, Stdin_Fd, Pipe_Output, Real_Stderr_Fd, Priority);
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
     (Args      : CList;
      Env       : CList;
      Cwd       : String                := "";
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority  : Priority_Class        := INHERIT)
      return Integer
   is
      Pid    : Handle;
      Result : Integer;
   begin
      Pid := Non_Blocking_Spawn
        (Args, Env, Cwd, Stdin_Fd, Stdout_Fd, Stderr_Fd, Priority);
      Result := Wait (Pid);
      return Result;
   end Blocking_Spawn;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Args      : CList;
      Env       : CList;
      Cwd       : String                := "";
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority  : Priority_Class        := INHERIT)
      return Handle
   is
      Result     : Handle;
      Arg_List   : CList;
      C_Cwd      : aliased String (1 .. Cwd'Length + 1);
      C_Cwd_Addr : System.Address := System.Null_Address;
      Success : Boolean;
      pragma Warnings (Off, Success);

      use OS.FS;
   begin
      Normalize_Arguments (Args, Arg_List);
      GNAT.Task_Lock.Lock;

      --  Prepare low-level argument list from the normalized arguments

      if Cwd /= "" then
         C_Cwd (Cwd'Range) := Cwd;
         C_Cwd (C_Cwd'Last) := ASCII.NUL;
         C_Cwd_Addr := C_Cwd'Address;
      end if;

      Set_Close_On_Exec (Stdin_Fd, False);
      Set_Close_On_Exec (Stdout_Fd, False);
      Set_Close_On_Exec (Stderr_Fd, False);

      Result := Portable_Execvp
        (As_C_String_Array (Arg_List),
         C_Cwd_Addr,
         As_C_String_Array (Env),
         Stdin_Fd, Stdout_Fd, Stderr_Fd, Priority);
      Set_Close_On_Exec (Stdin_Fd, True);
      Set_Close_On_Exec (Stdout_Fd, True);
      Set_Close_On_Exec (Stderr_Fd, True);
      GNAT.Task_Lock.Unlock;
      Deallocate (Arg_List);
      return Result;
   end Non_Blocking_Spawn;

   -------------------------
   -- Normalize_Arguments --
   -------------------------

   procedure Normalize_Arguments
     (Args      : CList;
      Norm_Args : in out CList)
   is

      procedure Quote_Argument (Arg : String);
      --  Add quote around argument if it contains spaces

      C_Argument_Needs_Quote : Integer;
      pragma Import (C, C_Argument_Needs_Quote, "__gnat_argument_needs_quote");
      Argument_Needs_Quote : constant Boolean := C_Argument_Needs_Quote /= 0;

      --------------------
      -- Quote_Argument --
      --------------------

      procedure Quote_Argument (Arg : String)
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
            Append (Norm_Args, Res (1 .. J));
         else
            Append (Norm_Args, Res (2 .. J - 1));
         end if;
      end Quote_Argument;

      --  Start of processing for Normalize_Arguments

   begin
      if Argument_Needs_Quote then
         for K in 1 .. Length (Args) loop
            Quote_Argument (Element (Args, K));
         end loop;
      else
         for K in 1 .. Length (Args) loop
            Append (Norm_Args, Element (Args, K));
         end loop;
      end if;
   end Normalize_Arguments;

end OS.Exec;
