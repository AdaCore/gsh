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

with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.Variables.Output is

   type Operator is (NULL_REDIR,
                     OPEN_READ,
                     OPEN_WRITE,
                     OPEN_APPEND,
                     DUPLICATE,
                     IOHERE);

   type Redirection (Kind : Operator := NULL_REDIR) is record
      case Kind is
         when OPEN_READ | OPEN_WRITE | OPEN_APPEND  =>
            Open_Target : Natural;
            Filename    : Token;
         when DUPLICATE =>
            Dup_Target  : Natural;
            Source      : Token;
         when IOHERE =>
            Doc_Target  : Natural;
            Content     : Token;
            Expand      : Boolean;
         when NULL_REDIR =>
            null;
      end case;
   end record;
   --  Redirection directive. F is the name of the file. If Append is True
   --  then F will be opened in append mode (relevant only for Stdin and
   --  Stdout).

   type Redirection_Stack is private;

   Empty_Redirections : constant Redirection_Stack;

   function Set_Redirections
     (S             : in out Shell_State;
      R             : Redirection_Stack;
      Free_Previous : Boolean := False)
     return Boolean;

   function Get_Redirections
     (S : Shell_State)
      return Shell_Descriptors;
   --  Return the current redirection set.

   procedure Restore_Redirections
     (S : in out Shell_State;
      R : Shell_Descriptors);
   --  Restore the previous redirections context.

   procedure Set_Pipe_Out (S : in out Shell_State);
   --  Set env to fill the pipe

   procedure Set_Pipe_In (S : in out Shell_State; Input_Fd : File_Descriptor);
   --  Set env to read the pipe

   procedure Close_Pipe (S : in out Shell_State);
   --  Close the pipe in the current context

   function Read_Pipe_And_Close
     (S : in out Shell_State;
      Input_Fd : File_Descriptor) return String;
   --  Read the pipe content, then close it, and return the content read.

   function Get_Fd
     (S : Shell_State; N : Integer) return File_Descriptor;

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

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean;
      Status        : out Boolean);

   procedure Push
     (RS : in out Redirection_Stack;
      R  : Redirection);

private
   type Redirection_Array is array (1 .. 16) of Redirection;
   type Redirection_Stack is record
      Top : Natural := 0;
      Ops : Redirection_Array;
   end record;
   --  Redirection directives for Stdin, Stdout and Stderr.

   Empty_Redirections : constant Redirection_Stack :=
     (0, (others => (Kind => NULL_REDIR)));
end Posix_Shell.Variables.Output;
