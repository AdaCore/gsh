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

with GNAT.Directory_Operations;
with System;
with GNAT.Task_Lock;
with GNAT.OS_Lib;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;

package body OS.FS is

   On_Windows : constant Boolean :=
     GNAT.Directory_Operations.Dir_Separator = '\';

   function To_C (Str : String) return String;
   --  Append to a string the nul character
   --
   --  Note that this is function is used to interface with C functions
   --  expecting char * parameters. You can pass the address of the first
   --  character to that string (i.e: S (S'First)'Address) to the C function.
   --  The rational to not use Interfaces.C transformation function is that it
   --  does not require the use of heap memory.
   --
   --  @param Str the input string
   --  @return same string with null character appended
   pragma Inline (To_C);

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
      function C_Close (FD : File_Descriptor) return int;
      pragma Import (C, C_Close, "close");

      Status : int;
      pragma Unreferenced (Status);
   begin
      Status := C_Close (FD);
   end Close;

   ---------------
   -- Copy_File --
   ---------------

   function Copy_File (Source              : String;
                       Target              : String;
                       Fail_If_Exists      : Boolean;
                       Preserve_Attributes : Boolean)
                       return Integer
   is
      pragma Warnings (Off);
      function Internal
        (Source              : chars_ptr;
         Target              : chars_ptr;
         Fail_If_Exists      : Boolean;
         Preserve_Attributes : Boolean)
         return unsigned_long;
      pragma Import (C, Internal, "__gsh_copy_file");
      pragma Warnings (On);
      Source_Ptr : chars_ptr := New_String (Source);
      Target_Ptr : chars_ptr := New_String (Target);
      Result     : unsigned_long;

   begin
      Result := Internal (Source_Ptr,
                          Target_Ptr,
                          Fail_If_Exists,
                          Preserve_Attributes);
      Free (Source_Ptr);
      Free (Target_Ptr);
      return Integer (Result);
   end Copy_File;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (Filename      : String;
      Ignore_Errors : Boolean := False)
   is
      Result : constant Integer := Delete_File (Filename);

   begin

      if Result /= 0 and not Ignore_Errors then
         raise OS_Error with "cannot delete file";
      end if;
   end Delete_File;

   -----------------
   -- Delete_File --
   -----------------

   function Delete_File (Filename : String) return Integer
   is
      function Delete_File_Internal
        (Path : System.Address)
         return long;
      pragma Import (C, Delete_File_Internal, "__gsh_unlink");

      Abs_Path : constant String := To_C
        (GNAT.OS_Lib.Normalize_Pathname
           (Filename,
            Resolve_Links => False));
      Result   : long;

   begin
      Result := Delete_File_Internal (Abs_Path (Abs_Path'First)'Address);
      return Integer (Result);
   end Delete_File;

   ---------
   -- Dup --
   ---------

   function Dup
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean := True)
     return File_Descriptor
   is
      function C_Dup (Fd : File_Descriptor) return File_Descriptor;
      pragma Import (C, C_Dup, "dup");

      Result : File_Descriptor;
   begin
      if FD = Invalid_FD then
         return FD;
      end if;

      if Close_On_Exec then
         GNAT.Task_Lock.Lock;
      end if;

      Result := C_Dup (FD);

      if Close_On_Exec then
         if Result >= 0 then
            Set_Close_On_Exec (Result, True);
         end if;
         GNAT.Task_Lock.Unlock;
      end if;

      if Result < 0 then
         raise OS_Error with "cannot duplicate file descriptor";
      end if;
      return Result;

   end Dup;

   ----------
   -- Dup2 --
   ----------

   procedure Dup2
     (From_FD       : File_Descriptor;
      Target_FD     : File_Descriptor;
      Close_On_Exec : Boolean := True)
   is
      function C_Dup2
        (Fd1 : File_Descriptor;
         Fd2 : File_Descriptor)
         return File_Descriptor;
      pragma Import (C, C_Dup2, "dup2");

      Result : File_Descriptor;
   begin
      if Close_On_Exec then
         GNAT.Task_Lock.Lock;
      end if;

      Result := C_Dup2 (From_FD, Target_FD);

      if Close_On_Exec then
         if Result >= 0 then
            Set_Close_On_Exec (Target_FD, True);
         end if;
         GNAT.Task_Lock.Unlock;
      end if;

      if Result < 0 then
         raise OS_Error with "cannote copy file descriptor";
      end if;

   end Dup2;

   ----------------
   -- Is_Console --
   ----------------

   function Is_Console (FD : File_Descriptor) return Boolean
   is
      function Is_Console_Internal
        (Fd : File_Descriptor)
         return Integer;
      pragma Import (C, Is_Console_Internal, "__gsh_is_console");

   begin
      return Is_Console_Internal (FD) /= 0;
   end Is_Console;

   ------------------
   -- Is_Null_File --
   ------------------

   function Is_Null_File (Filename : String) return Boolean is
   begin
      if Filename = Null_File then
         return True;
      else
         return False;
      end if;
   end Is_Null_File;

   ----------
   -- Join --
   ----------

   function Join
     (Left  : String;
      Right : String)
      return String
   is
   begin
      return Left & GNAT.Directory_Operations.Dir_Separator & Right;
   end Join;

   ---------------
   -- Move_File --
   ---------------

   function Move_File
     (Source : String;
      Target : String;
      Force  : Boolean)
      return Integer
   is
      pragma Warnings (Off);
      function Internal
        (Source    : chars_ptr;
         Target    : chars_ptr;
         Overwrite : Boolean)
         return unsigned_long;
      pragma Import (C, Internal, "__gsh_mv");
      pragma Warnings (On);
      Source_Ptr : chars_ptr := New_String (Source);
      Target_Ptr : chars_ptr := New_String (Target);
      Result     : unsigned_long;

   begin
      Result := Internal (Source_Ptr,
                          Target_Ptr,
                          Force);
      Free (Source_Ptr);
      Free (Target_Ptr);
      if Result = 0 then
         return 0;
      else
         return 1;
      end if;
   end Move_File;

   ---------------
   -- Null_File --
   ---------------

   function Null_File return String is
   begin
      if On_Windows then
         return "NUL";
      else
         return "/dev/null";
      end if;
   end Null_File;

   ----------
   -- Open --
   ----------

   function Open
     (Filename : String; Mode : File_Mode := Read_Mode)
      return File_Descriptor

   is
      Result  : File_Descriptor;

      function C_Open
        (Name  : System.Address;
         Fmode : File_Mode)
         return File_Descriptor;
      pragma Import (C, C_Open, "__gsh_open");

      C_Filename : constant String := To_C (Filename);
      Should_Delete : constant Boolean :=
        Mode = Write_Mode and then not Is_Null_File (Filename);
   begin

      --  In case we open a file in write mode with Append set to False, we
      --  could use the system call to do that. Instead we explicitely delete
      --  the file as on Windows our deletion method is more robust than the
      --  internal attempt to reset the file.
      if Should_Delete then
         GNAT.Task_Lock.Lock;
         begin
            if GNAT.OS_Lib.Is_Regular_File (Filename) then
               Delete_File (Filename);
            end if;
         exception
            when OS_Error =>
               GNAT.Task_Lock.Unlock;
               raise OS_Error with "cannot open " & Filename &
                 " (cannot reset content)";
         end;
      end if;

      Result := C_Open (C_Filename (C_Filename'First)'Address, Mode);

      if Should_Delete then
         GNAT.Task_Lock.Unlock;
      end if;

      if Result = -1 then
         --  cannot open the file
         raise OS_Error with "cannot open " & Filename;
      end if;

      return Result;
   end Open;

   ---------------
   -- Open_Pipe --
   ---------------

   procedure Open_Pipe (Pipe_Input  : out File_Descriptor;
                        Pipe_Output : out File_Descriptor)
   is
      type Pipe_Type is record
         Input, Output : OS.FS.File_Descriptor;
      end record;

      function Create_Pipe (Pipe : not null access Pipe_Type) return Integer;
      pragma Import (C, Create_Pipe, "__gnat_pipe");

      Status : Integer;
      Result : aliased Pipe_Type;
   begin
      GNAT.Task_Lock.Lock;
      Status := Create_Pipe (Result'Access);

      if Status /= 0 then
         GNAT.Task_Lock.Unlock;
         raise OS_Error with "cannot open pipe";
      end if;

      Pipe_Input := Result.Input;
      Pipe_Output := Result.Output;

      begin
         Set_Close_On_Exec (Pipe_Input, True);
         Set_Close_On_Exec (Pipe_Output, True);
      exception
         when OS_Error =>
            GNAT.Task_Lock.Unlock;
            raise;
      end;
      GNAT.Task_Lock.Unlock;

   end Open_Pipe;

   ----------
   -- Read --
   ----------

   function Read (FD : File_Descriptor; Buffer : in out String) return Integer
   is
      function C_Read
        (Fd     : File_Descriptor;
         Buffer : System.Address;
         Size   : size_t)
         return int;
      pragma Import (C, C_Read, "read");

      Result : int;
   begin
      Result := C_Read (FD, Buffer (Buffer'First)'Address, Buffer'Length);
      if Result < 0 then
         raise OS_Error with "read error";
      end if;

      return Integer (Result);
   end Read;

   ----------
   -- Read --
   ----------

   function Read (FD : File_Descriptor) return GNAT.Strings.String_Access
   is
      use GNAT.Strings;
      Length : constant Long_Integer := File_Length (FD);
      Buffer : constant String_Access := new String (1 .. Integer (Length));
      N      : Integer;
   begin
      if Length = 0 then
         return Buffer;
      end if;

      N := Read (FD, Buffer.all);
      if N /= Integer (Length) then
         raise OS_Error with "cannot read complete file content";
      end if;
      return Buffer;
   end Read;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (P   : String;
                           Dir : String;
                           Path_Prefix : String := ".") return String is
   begin

      if P = Dir then
         return Path_Prefix;

      elsif P'Length > Dir'Length then

         if P (P'First .. Dir'Length) = Dir then

            if P (Dir'Length + 2 .. P'Last) = Path_Prefix then
               return Path_Prefix;

            elsif P (Dir'Length + 2 .. P'Last) = Path_Prefix & "/." then
               return Path_Prefix;

            elsif Dir'Length + 2 + Path_Prefix'Length < P'Last then
               if P (Dir'Length + 2 ..
                       Dir'Length + 1 + Path_Prefix'Length) = Path_Prefix
               then
                  return P (Dir'Length + 2 .. P'Last);
               else
                  return Path_Prefix & "/" & P (Dir'Length + 2 .. P'Last);
               end if;
            else
               return Path_Prefix & "/" & P (Dir'Length + 2 .. P'Last);
            end if;

         else
            return P;
         end if;
      else
         return P;
      end if;
   end Relative_Path;

   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (FD            : OS.FS.File_Descriptor;
      Close_On_Exec : Boolean)
   is
      function C_Set_Close_On_Exec
        (FD            : OS.FS.File_Descriptor;
         Close_On_Exec : Integer)
         return Integer;
      pragma Import (C, C_Set_Close_On_Exec, "__gsh_set_close_on_exec");

      Status : Integer;
   begin
      --  Do nothing if FD is invalid
      if FD < 0 then
         return;
      end if;

      Status := C_Set_Close_On_Exec (FD, Boolean'Pos (Close_On_Exec));
      if Status /= 0 then
         --  On Windows at least we cannot set close_on_exec on console file
         --  file descriptor. So in that case ignore the error.
         if not Is_Console (FD) then
            raise OS_Error
              with "cannot set close_on_exec flag on " & FD'Img;
         end if;
      end if;
   end Set_Close_On_Exec;

   ----------
   -- To_C --
   ----------

   function To_C (Str : String) return String is
      Result : String (1 .. Str'Length + 1);
   begin
      Result (1 .. Str'Length) := Str;
      Result (Result'Last) := ASCII.NUL;
      return Result;
   end To_C;

   -----------
   -- Write --
   -----------

   procedure Write (FD : File_Descriptor; Buffer : String)
   is
      function C_Write
        (Fd     : File_Descriptor;
         Buffer : System.Address;
         Size   : size_t)
         return int;
      pragma Import (C, C_Write, "write");

      Written : int;
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      Written := C_Write (FD, Buffer (Buffer'First)'Address, Buffer'Length);
      if Integer (Written) /= Buffer'Length then
         raise OS_Error with "write error";
      end if;
   end Write;

end OS.FS;
