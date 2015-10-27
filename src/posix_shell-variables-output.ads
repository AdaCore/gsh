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

with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.Variables.Output is

   function Apply_Redirections
     (State        : in out Shell_State;
      Redirections : Redirection_Stack;
      In_Place     : Boolean := False)
      return Boolean;
   --  Apply redirections to the current file descriptors.
   --
   --  @param State current shell state
   --  @param Redirections a list of redirections operations
   --  @param In_Place
   --      if True then file descriptors that are overriden by redirections
   --      operations are closed. This means that we cannot restore the
   --      previous state afterwards. The In_Place is currently used only in
   --      the context of the ``exec`` builtin
   --  @return True if the operation is successfull, False otherwise

   function Get_Descriptors (State : Shell_State) return Shell_Descriptors;
   --  Retrieves the current list of file descriptors.
   --
   --  @param State current shell state
   --  @return the list of file descriptors currently in use

   procedure Restore_Descriptors
     (State       : in out Shell_State;
      Descriptors : Shell_Descriptors;
      In_Place    : Boolean := False);
   --  Restore the previous descriptors.
   --
   --  @param State current shell state
   --  @param Descriptors
   --      list of file descriptors we want to restore. This
   --      list usually comes from a call to Get_Descriptors just before a
   --      call to Apply_Redirections
   --  @param In_Place
   --      if True then the call is a nop operation. A serie of call to
   --      Apply_Redirections/Restore_Descriptors should use the same
   --      value for the In_Place parameter.

   procedure Set_Pipe_Out (S : in out Shell_State);
   --  Set env to fill the pipe

   procedure Set_Pipe_In (S        : in out Shell_State;
                          Input_Fd : OS.FS.File_Descriptor);
   --  Set env to read the pipe

   procedure Close_Pipe (S : in out Shell_State);
   --  Close the pipe in the current context

   function Read_Pipe_And_Close
     (S : in out Shell_State;
      Input_Fd : OS.FS.File_Descriptor) return String;
   --  Read the pipe content, then close it, and return the content read.

   function Get_Fd
     (S : Shell_State; N : Integer) return OS.FS.File_Descriptor;

   procedure Close (S : Shell_State; N : Integer);

   procedure Put (S : Shell_State; IO : Integer; Str : String);
   --  Print S in the given IO descriptor (no new line added).
   --  For IO Usually, 0 is stdin, 1 is stdout, and 2 is stderr.

   function Read (S : Shell_State; IO : Integer) return String;

   function Read (S : Shell_State; IO : Integer) return Character;

   procedure New_Line (S : Shell_State; IO : Integer);
   --  Print a new-line in the given IO descriptor.

   procedure Error (S : Shell_State; Msg : String);
   --  Print an error message on standard error.

   procedure Warning (S : Shell_State; Msg : String);
   --  Print a warning message on standard error.

private

end Posix_Shell.Variables.Output;
