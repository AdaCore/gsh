with OS;
with GNAT.OS_Lib;
with Test_Assert;
with GNAT.Strings;

with OS.FS; use OS.FS;

function Test return Integer 
is
   package A renames Test_Assert;
   package GS renames GNAT.Strings;

   procedure Test_Open is
      Fd : File_Descriptor;
      Str : GS.String_Access;
   begin 
      Fd := Open ("./exchange_file.txt", Mode => Write_Mode);
      Write (Fd, "exchange text");
      Close (Fd);
      Fd := Open ("./exchange_file.txt", Mode => Append_Mode);
      Write (Fd, " and more");
      Close (Fd);
      Fd := Open ("./exchange_file.txt", Mode => Read_Mode);
      Str := Read (Fd);
      Close (Fd);
      A.Assert (Str.all, "exchange text and more", "test os.fs.open");

      Fd := Open ("./exchange_file.txt", Mode => Write_Mode);
      Write (Fd, "reset content");
      Close (Fd);

      Fd := Open ("./exchange_file.txt", Mode => Read_Mode);
      Str := Read (Fd);
      Close (Fd);
      A.Assert (Str.all, "reset content",
                "check that os.fs.open in write mode reset content");

      begin
         Fd := Open ("./non_existing_file.txt", Mode => Read_Mode);
         A.Assert (False, "no OS_Error exception");
      exception
         when OS.OS_Error =>
            A.Assert (True, "got OS_Error if file does not exist");
         when others =>
            A.Assert (False, "wrong exception");
      end;
   end Test_Open;

   procedure Test_Copy_File is
      Fd : File_Descriptor;
      Result : Integer;
      Orig_Path : String := GNAT.OS_Lib.Normalize_Pathname ("./copy_fie_orig.txt");
      Copy_Path : String := GNAT.OS_Lib.Normalize_Pathname ("./copy_fie_copy.txt");
   begin

      Fd := Open (Orig_Path, Mode => Write_Mode);
      Write (Fd, "exchange text");
      Close (Fd);

      Result := Copy_File (Orig_Path,
                              Copy_Path,
                              Fail_If_Exists => True,
                              Preserve_Attributes => True);
      A.Assert (Result = 0, "copy file test" & Result'Img);
      Result := Copy_File (Orig_Path,
                              Copy_Path & "2",
                              Fail_If_Exists => True,
                              Preserve_Attributes => False);
      A.Assert (Result = 0, "copy file test" & Result'Img);
   end Test_Copy_File;

   procedure Test_Dup is
      Fd  : File_Descriptor := Invalid_FD;
      Fd2 : File_Descriptor;
   begin
      Fd2 := Dup (FD);
      A.Assert (Fd2 = Invalid_FD, "duplicate invalid fd");
      Fd2 := Dup (Standout);
      A.Assert (Fd2 > 0, "valid duplicate fd");
   end Test_Dup;

   procedure Test_Dup2 is
      Fd  : File_Descriptor := Invalid_FD;
      Fd2 : File_Descriptor := 25;
   begin
      Dup2 (Standout, Fd2);
      A.Assert (Fd2 > 0, "valid duplicate fd");
   end Test_Dup2;

   procedure Test_Delete
   is
      Fd : File_Descriptor;
      Path : String := "./test_delete.txt";
   begin
      Fd := Open (Path, Mode => Write_Mode);
      begin
         Delete_File (Path);
         A.Assert (False, "exception expected");
      exception
         when OS.OS_Error =>
            A.Assert (True, "OS_Error should be raised when file cannot be deleted");
         when others =>
            A.Assert (False, "OS_Error should be raised when file cannot be deleted");
      end;
      Close (Fd);
      Delete_File (Path);
   end Test_Delete;

   ------------------------
   -- Test_Relative_Path --
   ------------------------
   
   procedure Test_Relative_Path is
   begin
      A.Assert (Relative_Path ("C:/dir2/file1.txt", "C:/dir2"),
                "./file1.txt",
                "basic relative path test");
   end Test_Relative_Path;

   ---------------
   -- Test_Pipe --
   ---------------
   
   procedure Test_Pipe is
      Fd1, Fd2 : File_Descriptor;
      Buffer_Last : Integer;
      Buffer : String (1 .. 2048);
   begin
      Open_Pipe (Fd2, Fd1);
      Write (Fd1, "pipe data");
      Close (Fd1);
      Buffer_Last := Read (Fd2, Buffer);
      A.Assert (Buffer (1 .. Buffer_Last), "pipe data", "basic pipe test");
      Close (Fd2);
   end Test_Pipe;

begin
   Test_Open;
   Test_Copy_File;
   Test_Dup;
   Test_Dup2;
   Test_Delete;
   Test_Relative_Path;
   Test_Pipe;
   return A.Report;
end Test;
