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

with Sh.States; use Sh.States;
with Sh.Tokens; use Sh.Tokens;

package Sh.States.IO is

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
   --  @return True if the operation is successful, False otherwise

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

   procedure Set_Descriptor
     (State : in out Shell_State;
      IO    : Integer;
      Fd    : OS.FS.File_Descriptor);
   --  Override file descriptor associated with IO number IO
   --
   --  @param State the current shell state
   --  @param IO
   --      the IO number to be changed. If the IO number was associated with
   --      an opened file the previous file descriptor might be closed.
   --  @param Fd file descriptor to associate with IO

   function Read_Pipe_And_Close
     (S        : in out Shell_State;
      Input_Fd : OS.FS.File_Descriptor) return String;
   --  Read the pipe content, then close it, and return the content read.
   --  ??? should be move outside (no need for shell state here)

   function Get_File_Descriptor
     (State : Shell_State;
      IO    : Integer)
      return OS.FS.File_Descriptor;
   --  Get real file descriptor associated with IO number
   --
   --  @param State the shell state
   --  @param IO IO number to retrieve
   --  @return a file descriptor. This can be Invalid_FD.

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

end Sh.States.IO;
