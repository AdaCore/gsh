with GNAT.OS_Lib; use GNAT.OS_Lib;

package Posix_Shell.Exec is

   type Handle is mod 2 ** Standard'Address_Size;

   function Non_Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String;
      Env       : Argument_List;
      Stdin_Fd  : File_Descriptor;
      Stdout_Fd : File_Descriptor;
      Stderr_Fd : File_Descriptor)
      return Handle;
   --  Launch a process but do not wait for its termination

   function Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String;
      Env       : Argument_List;
      Stdin_Fd  : File_Descriptor;
      Stdout_Fd : File_Descriptor;
      Stderr_Fd : File_Descriptor)
      return Integer;
   --  Launch a process and wait until it finishes

   function Blocking_Spawn
     (Args      : Argument_List;
      Cwd       : String;
      Env       : Argument_List)
      return Integer;
   --  Same as above, but reusing the current file descriptors.

   procedure Shell_Exit (Code : Integer);
   pragma No_Return (Shell_Exit);
   --  Causes the shell to exit with the given error code.

   Shell_Exit_Exception : exception;
   --  An exception signaling that we need to exit the current shell.
   --  At the time when this exception is raised, the exit status has
   --  already been saved.

   Shell_Return_Exception : exception;
   --  An exception signaling that we need to return from the current
   --  shell.  At the time this exception is raised, the exit status
   --  has already been saved.

end Posix_Shell.Exec;
