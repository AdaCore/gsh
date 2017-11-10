with Test_Assert;
with Sh.Tokens.Lexer; use Sh.Tokens.Lexer;
with Sh.Tokens; use Sh.Tokens;
with Ada.Command_Line; use Ada.Command_Line;
with Sh;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;

   Buffer : Token_Buffer := New_Buffer_From_File(Argument (1));
   Tk : Token;
   use type Sh.Token_Type;
begin
   loop
      Tk := Read_Command_Token (Buffer);
      Ada.Text_IO.Put_Line (Image (Tk));
      exit when Kind (Tk) = Sh.T_EOF;
   end loop;
   return A.Report;
end Test;
