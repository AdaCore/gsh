with Ada.Finalization;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Posix_Shell.Buffers is

   type Buffer is private;

   type Text_Position is private;
   Null_Text_Position : constant Text_Position;

   function Get_Pos (T : Text_Position) return Natural;
   pragma Inline (Get_Pos);

   function Get_Lineno (T : Text_Position) return Natural;
   function Get_Columno (T : Text_Position) return Natural;
   function Image (T : Text_Position) return String;

   procedure Rewind  (B : in out Buffer; Step : Positive := 1);
   procedure Forward (B : in out Buffer; Step : Positive := 1);
   procedure Set_Pos (B : in out Buffer; P : Text_Position);

   function Current (B : Buffer) return Character;
   function Current (B : Buffer; Size : Positive) return String;

   function Current_Pos (B : Buffer) return Text_Position;
   function Current_Pos (B : Buffer) return Natural;
   function Current_Lineno (B : Buffer) return Natural;
   function Current_Columnno (B : Buffer) return Natural;

   function New_Buffer (Str : String) return Buffer;
   function New_Buffer_From_File (Filename : String) return Buffer;

private
   use Ada.Finalization;

   type Text_Position is record
      Pos    : Natural;
      Column : Natural;
      Line   : Natural;
   end record;

   Null_Text_Position : constant Text_Position := (0, 0, 0);

   type Natural_Array is array (Positive range <>) of Positive;
   type Natural_Array_Access is access Natural_Array;

   type Buffer is new Controlled with record
      S     : String_Access;
      Pos   : Text_Position;
      Lines : Natural_Array_Access;
   end record;

   pragma Finalize_Storage_Only (Buffer);

   procedure Initialize (Object : in out Buffer);
   procedure Adjust     (Object : in out Buffer);
   procedure Finalize   (Object : in out Buffer);

end Posix_Shell.Buffers;
