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

with GNAT.Strings;
with Ada.Strings.Unbounded;
with OS.FS;

package OS.Exec is

   subtype Argument_List is GNAT.Strings.String_List;

   Null_Argument_List : constant Argument_List;

   type Handle is private;
   --  Private type that map to a process "handle", the type in fact maps to
   --  different type of object depending on the system. On Unix system it will
   --  be usually a process id and on Windows system a handle.

   type Priority_Class is
     (INHERIT,
      IDLE,
      BELOW_NORMAL,
      NORMAL,
      ABOVE_NORMAL,
      HIGH);

   function Non_Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String                := "";
      Env       : Argument_List         := Null_Argument_List;
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority  : Priority_Class        := INHERIT)
      return Handle;
   --  Launch a process in background.
   --
   --  @param Args command line arguments including the command line itself
   --  @param Cwd
   --      directory in which the process is launched. If set to "" then
   --      current directory of the current process is used
   --  @param Env
   --      environment to be passed to the process. This is a list of of string
   --      which should have the following format "VARIABLE=value". If the list
   --      is Null_Argument_List then the current environment will be used.
   --  @param Stdin_Fd Standard input of the spawned process
   --  @param Stdout_Fd likewise for standard output
   --  @param Stderr_Fd likewise for standard error
   --  @param Priority
   --      set process priority. If INHERIT then use current process priority.
   --  @return a handle to the spawned process

   function Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String                := "";
      Env       : Argument_List         := Null_Argument_List;
      Stdin_Fd  : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout_Fd : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr_Fd : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority  : Priority_Class        := INHERIT)
      return Integer;
   --  Launch a process and return its exit status code.
   --
   --  See Non_Blocking_Spawn for parameters documentation
   --
   --  @return the exit status of the process

   function Blocking_Spawn
     (Args            : Argument_List;
      Cwd             : String                := "";
      Env             : Argument_List         := Null_Argument_List;
      Stdin_Fd        : OS.FS.File_Descriptor := OS.FS.Standin;
      Stderr_Fd       : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority        : Priority_Class        := INHERIT;
      Status          : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Launch a process and return output and exit code
   --
   --  See Non_Blocking_Spawn for Args, Cwd, End and Stdin_Fd
   --
   --  Note that we use an unbounded string rather than a string to avoid
   --  stack issues in case the process output size is large
   --
   --  @param Stderr_Fd
   --      file descriptor for error stream. Note that if it is set to
   --      To_Stdout then error stream will be redirected to the output stream
   --      i.e merged into the process output
   --  @param Status exit status of the process
   --  @return output of the process

   function Wait (H : Handle) return Integer;
   pragma Import (C, Wait, "__gsh_waitpid");
   --  Wait for a process end, and return its exit code
   --
   --  @param Handle a handle as returned by Non_Blocking_Spawn
   --  @return the exit status
private

   Null_Argument_List : constant Argument_List := (1 .. 0 => null);

   type Handle is mod 2 ** Standard'Address_Size;

end OS.Exec;
