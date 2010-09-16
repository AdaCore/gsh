with Ada.Unchecked_Deallocation;
with Posix_Shell.Output; use Posix_Shell.Output;
with Posix_Shell.Utils; use Posix_Shell.Utils;

package body Posix_Shell.Buffers is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Buffer) is
   begin
      if Object.S /= null then
         Object.S := new String'(Object.S.all);
         Object.Lines := new Natural_Array'(Object.Lines.all);
      end if;
   end Adjust;

   -------------
   -- Current --
   -------------

   function Current (B : Buffer) return Character is
   begin
      if B.Pos.Pos > B.S'Last then
         return ASCII.EOT;
      end if;

      return B.S (B.Pos.Pos);
   end Current;

   -------------
   -- Current --
   -------------

   function Current (B : Buffer; Size : Positive) return String is
   begin
      if B.Pos.Pos > B.S'Last then
         return "";
      end if;

      if B.Pos.Pos + Size - 1 > B.S'Last then
         return B.S (B.Pos.Pos .. B.S'Last);
      end if;

      return B.S (B.Pos.Pos .. B.Pos.Pos + Size - 1);
   end Current;

   ----------------------
   -- Current_Columnno --
   ----------------------

   function Current_Columnno (B : Buffer) return Natural is
   begin
      return B.Pos.Column;
   end Current_Columnno;

   --------------------
   -- Current_Lineno --
   --------------------

   function Current_Lineno (B : Buffer) return Natural is
   begin
      return B.Pos.Line;
   end Current_Lineno;

   -----------------
   -- Current_Pos --
   -----------------

   function Current_Pos (B : Buffer) return Text_Position is
   begin
      return B.Pos;
   end Current_Pos;

   -----------------
   -- Current_Pos --
   -----------------

   function Current_Pos (B : Buffer) return Natural is
   begin
      return B.Pos.Pos;
   end Current_Pos;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Buffer) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Natural_Array,
           Natural_Array_Access);
   begin
      if Object.S /= null then
         Free (Object.S);
         Deallocate (Object.Lines);
         Object.S := null;
         Object.Lines := null;
         Object.Pos := (0, 0, 0);
      end if;
   end Finalize;

   -------------
   -- Forward --
   -------------

   procedure Forward (B : in out Buffer; Step : Positive := 1) is
      Tmp : Natural := B.Pos.Pos;
   begin
      for J in 1 .. Step loop
         exit when Tmp > B.S'Last;
         if B.S (Tmp) = ASCII.LF then
            B.Pos.Line := B.Pos.Line + 1;
         end if;
         Tmp := Tmp + 1;
      end loop;
      B.Pos.Pos := B.Pos.Pos + Step;
      if B.Pos.Pos > B.S'Last then
         B.Pos.Column := 1;
      else
         B.Pos.Column := B.Pos.Pos - B.Lines (B.Pos.Line) + 1;
      end if;
   end Forward;

   -----------------
   -- Get_Columno --
   -----------------

   function Get_Columno (T : Text_Position) return Natural is
   begin
      return T.Column;
   end Get_Columno;

   ----------------
   -- Get_Lineno --
   ----------------

   function Get_Lineno (T : Text_Position) return Natural is
   begin
      return T.Line;
   end Get_Lineno;

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos (T : Text_Position) return Natural is
   begin
      return T.Pos;
   end Get_Pos;

   -----------
   -- Image --
   -----------

   function Image (T : Text_Position) return String is
   begin
      return To_String (T.Line) & ":" & To_String (T.Column);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Buffer) is
   begin
      Object.S := null;
      Object.Pos := (0, 0, 0);
      Object.Lines := null;
   end Initialize;

   ----------------
   -- New_Buffer --
   ----------------

   function New_Buffer (Str : String) return Buffer is
      Result : Buffer;
      Buffer_Str : constant String := Strip_CR (Str);
      Current_Line : Positive := 1;
      Lines : Natural_Array (1 .. Str'Length + 1);
   begin
      Result.S := new String'(Buffer_Str);
      Result.Pos := (1, 1, 1);
      Lines (1) := 1;
      for J in Buffer_Str'Range loop
         if Buffer_Str (J) = ASCII.LF then
            if J < Buffer_Str'Last then
               Current_Line := Current_Line + 1;
               Lines (Current_Line) := J + 1;
            end if;
         end if;
      end loop;
      Result.Lines := new Natural_Array'(Lines (1 .. Current_Line));
      return Result;
   end New_Buffer;

   --------------------------
   -- New_Buffer_From_File --
   --------------------------

   function New_Buffer_From_File (Filename : String) return Buffer is
      F : constant File_Descriptor := Open_Read (Filename, Binary);
      Expected_Bytes_Read : Integer;
      Bytes_Read : Integer;
   begin
      if F = Invalid_FD then
         --  The Open_Read failed.
         Error (Filename & ": Unable to open for reading");
         raise Buffer_Read_Error;
      end if;

      Expected_Bytes_Read := Integer (File_Length (F));

      declare
         Buffer_Str : aliased String (1 .. Expected_Bytes_Read);
      begin
         Bytes_Read := Read (F, Buffer_Str'Address, Expected_Bytes_Read);
         Close (F);

         if Bytes_Read /= Expected_Bytes_Read then
            Error (Filename & ": Read " & To_String (Bytes_Read)
                   & " bytes (expected " & To_String (Expected_Bytes_Read)
                   & " bytes).");
            raise Buffer_Read_Error;
         end if;

         return New_Buffer (Buffer_Str);
      end;
   end New_Buffer_From_File;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (B : in out Buffer; Step : Positive := 1) is
   begin
      if B.Pos.Pos - Step < 1 then
         B.Pos := (1, 1, 1);
         return;
      end if;

      for J in 1 .. Step loop
         if B.Pos.Pos - J <=  B.S'Last and then
           B.S (B.Pos.Pos - J) = ASCII.LF
         then
            B.Pos.Line := B.Pos.Line - 1;
         end if;
      end loop;
      B.Pos.Pos := B.Pos.Pos - Step;
      if B.Pos.Pos > B.S'Last then
         B.Pos.Column := 1;
      else
         B.Pos.Column := B.Pos.Pos - B.Lines (B.Pos.Line) + 1;
      end if;
   end Rewind;

   -------------
   -- Set_Pos --
   -------------

   procedure Set_Pos (B : in out Buffer; P : Text_Position) is
   begin
      B.Pos := P;
   end Set_Pos;

end Posix_Shell.Buffers;
