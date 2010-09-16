with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;

with GNAT.OS_Lib; use GNAT.OS_Lib;

pragma Elaborate_All (Posix_Shell.Annotated_Strings);

package Posix_Shell.Output is

   type Redir_Cmd is (NULL_REDIR,
                      OPEN_READ,
                      OPEN_WRITE,
                      OPEN_APPEND,
                      DUPLICATE,
                      IOHERE);

   type Redirection_Op is record
      Target_FD : Natural;
      Cmd       : Redir_Cmd;
      Source_FD : Natural;
      Filename  : Annotated_String := Null_Annotated_String;
   end record;
   --  Redirection directive. F is the name of the file. If Append is True
   --  then F will be opened in append mode (relevant only for Stdin and
   --  Stdout).

   type Redirection_Op_Array is array (1 .. 16) of Redirection_Op;
   type Redirection_Op_Stack is record
      Top : Natural := 0;
      Ops : Redirection_Op_Array;
   end record;
   --  Redirection directives for Stdin, Stdout and Stderr.

   Empty_Redirection_Op_Stack : Redirection_Op_Stack :=
     (0, (others => (0, NULL_REDIR, 0, Null_Annotated_String)));

   type Redirection_State is record
      Fd       : File_Descriptor;
      Filename : String_Access;
      Delete_On_Close : Boolean;
   end record;
   --  State of Stdin, Stdout or Stderr (file descriptor and filename
   --  if relevant).

   type Redirection_States is array (-2 .. 13) of Redirection_State;
   --  State of Stdin, Stdout and Stderr.
   --  ??? brobecker/2007-04-23:
   --  ???    I think that 0 .. 2 are stdin, stdout and stderr,
   --  ???    and 3 .. 4 are pipe-in and pipe-out.

   procedure Push_Redirections (R : Redirection_Op_Stack);
   --  Set a new redirection context.

   procedure Set_Redirections (R : Redirection_Op_Stack);

   function Get_Current_Redirections return Redirection_States;
   --  Return the current redirection set.

   procedure Pop_Redirections;
   --  Restore the previous redirections context.

   procedure Set_Pipe_Out;
   --  Set env to fill the pipe

   procedure Set_Pipe_In;
   --  Set env to read the pipe

   procedure Close_Pipe;
   --  Close the pipe in the current context

   function Read_Pipe_And_Close return String;
   --  Read the pipe content, then close it, and return the content read.

   procedure Put (IO : Integer; S : String);
   --  Print S in the given IO descriptor (no new line added).
   --  For IO Usually, 0 is stdin, 1 is stdout, and 2 is stderr.

   function Read (IO : Integer) return String;

   function Read (IO : Integer) return Character;

   procedure New_Line (IO : Integer);
   --  Print a new-line in the given IO descriptor.

   procedure Error (Msg : String);
   --  Print an error message on standard error.

   procedure Warning (Msg : String);
   --  Print a warning message on standard error.

end Posix_Shell.Output;
