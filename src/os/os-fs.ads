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

package OS.FS is

   type File_Descriptor is new Integer;
   --  Corresponds to the int file handle values used in the C routines

   Standin  : constant File_Descriptor := 0;
   Standout : constant File_Descriptor := 1;
   Standerr : constant File_Descriptor := 2;
   --  File descriptors for standard input output files

   Invalid_FD : constant File_Descriptor := -1;
   --  File descriptor returned when error in opening/creating file

   function Is_Null_File (Filename : String) return Boolean;
   pragma Inline (Is_Null_File);
   --  Return true if the path correspond to the system 'null' file
   --
   --  @param Filename path to check
   --  @return True if this is the system null file

   function Null_File return String;
   pragma Inline (Null_File);
   --  Return the path to the system null file
   --
   --  @param Filename a path
   --  @return True if this is the system null file, False otherwise

   type File_Mode is (Read_Mode, Write_Mode, Append_Mode);
   for File_Mode'Size use Integer'Size;
   for File_Mode use (Read_Mode => 0, Write_Mode => 1, Append_Mode => 2);

   function Open
     (Filename : String; Mode : File_Mode := Read_Mode)
      return File_Descriptor;
   --  Open a file
   --
   --  Note the file is always opened in binary mode which means that line
   --  endings are not translated. Also the file descriptor is marked as non
   --  inheritable (i.e close on exec on Unix system). The function is
   --  thread-safe, providing you use only this package to access files.
   --
   --  @param Filename path of the file to be opened
   --  @param File_Mode
   --      if set to Read_Mode the file is opened in read-only mode. When using
   --      Write_Mode the file is opened for writing and content is reset. In
   --      Append_Mode the is either created or opened with offset initialize
   --      to the end of the file
   --  @return a file descriptor
   --  @raise OS_Error in case the file cannot be opened

   procedure Write (FD : File_Descriptor; Buffer : String);
   --  Write a string into a file
   --
   --  @param FD a valid file descriptor
   --  @param Buffer he data to be written
   --  @raise OS_Error if the not all the data can be written

   function Read (FD : File_Descriptor; Buffer : in out String) return Integer;

   procedure Close (FD : File_Descriptor);
   pragma Inline (Close);
   --  Close a file descriptor
   --
   --  @param FD the file descriptor to be closed

   procedure Delete_File (Filename : String);
   --  Delete a file
   --
   --  @param Filename path to the file to be deleted
   --  @raise OS_Error in case the file cannot be deleted

   function Dup
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean := True)
      return File_Descriptor;
   --  Duplicate a file descriptor
   --
   --  @param Fd file descriptor
   --  @param Close_On_Exec
   --      if True (default) the new file descriptor will not be inherited by
   --      child processes
   --  @return the duplicate file descriptor
   --  @raise OS_Error in case the operation fails

   procedure Dup2
     (From_FD       : File_Descriptor;
      Target_FD     : File_Descriptor;
      Close_On_Exec : Boolean := True);
   --  Copy a file descriptor into another
   --
   --  @param From_Fd the file descriptor to be copied
   --  @param Target_Fd
   --      the file descriptor in which From_Fd is copied. If Target_Fd refers
   --      to an already opened file descriptor then it will be closed first
   --  @parm Close_On_Exec if True the new descriptor will not be inheritable
   --  @raise OS_Error in case the operation fails

   procedure Set_Close_On_Exec
     (FD            : OS.FS.File_Descriptor;
      Close_On_Exec : Boolean);
   --  Mark a file to be closed on execution or not
   --
   --  @param FD The file for which the status should be changed
   --  @param Close_On_Exec if True file will not inherited by child processes
   --      otherwise it will
   --  @raise OS_Error if the operation fails

end OS.FS;
