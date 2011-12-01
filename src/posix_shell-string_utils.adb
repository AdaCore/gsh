package body Posix_Shell.String_Utils is

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (S : String; Sub : String) return Boolean is
   begin
      if S'Length >= Sub'Length and then
        S (S'First .. S'First + Sub'Length - 1) = Sub
      then
         return True;
      else
         return False;
      end if;
   end Starts_With;

   ---------------
   -- Last_Line --
   ---------------

   function Last_Lines
     (S : String; Size : Natural) return String
   is
      Pos : Integer := S'Last;
      Newline_Found : Integer := 0;
   begin
      while Newline_Found < Size + 1 and then Pos >= S'First loop
         if S (Pos) = ASCII.LF then
            Newline_Found := Newline_Found + 1;
         end if;
         Pos := Pos - 1;
      end loop;

      --  Ignore last LF found
      if Newline_Found = Size + 1 then
         Pos := Pos + 1;
      end if;

      return S (Pos + 1 .. S'Last);
   end Last_Lines;

   ---------------
   -- From_Line --
   ---------------

   function From_Line
     (S : String; LineNo : Natural) return String
   is
      Pos : Integer := S'First;
      Current_Line : Integer := 1;
   begin
      while Current_Line < LineNo and then Pos <= S'Last loop
         if S (Pos) = ASCII.LF then
            Current_Line := Current_Line + 1;
         end if;

         Pos := Pos + 1;
      end loop;

      return S (Pos .. S'Last);
   end From_Line;

   -------------
   -- To_Line --
   -------------

   function To_Line
     (S : String; LineNo : Natural) return String
   is
      Pos : Integer := S'First;
      Newline_Found : Integer := 0;
   begin
      while Newline_Found < LineNo + 1 and then Pos <= S'Last loop
         if S (Pos) = ASCII.LF then
            Newline_Found := Newline_Found + 1;
         end if;

         Pos := Pos + 1;
      end loop;

      if Newline_Found = LineNo + 1 then
         Pos := Pos - 1;
      end if;

      return S (S'First .. Pos - 1);
   end To_Line;
end Posix_Shell.String_Utils;
