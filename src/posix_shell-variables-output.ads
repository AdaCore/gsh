with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Variables.Output is

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

   procedure Set_Redirections
     (S : Shell_State_Access;
      R : Redirection_Op_Stack;
      Free_Previous : Boolean := False);

   function Get_Redirections
     (S : Shell_State)
      return Redirection_States;
   --  Return the current redirection set.

   procedure Restore_Redirections
     (S : in out Shell_State;
      R : Redirection_States);
   --  Restore the previous redirections context.

   procedure Set_Pipe_Out (S : in out Shell_State);
   --  Set env to fill the pipe

   procedure Set_Pipe_In (S : in out Shell_State; Input_Fd : File_Descriptor);
   --  Set env to read the pipe

   procedure Close_Pipe (S : in out Shell_State);
   --  Close the pipe in the current context

   function Read_Pipe_And_Close
     (S : Shell_State_Access;
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

end Posix_Shell.Variables.Output;
