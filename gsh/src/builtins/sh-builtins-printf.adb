------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Printf                        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Sh.States.IO; use Sh.States.IO;

package body Sh.Builtins.Printf is

   function Printf_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      Format_String : constant String := Element (Args, 1);
      Output        : Unbounded_String := To_Unbounded_String ("");
      Index         : Integer := Format_String'First;
      CC            : Character := Format_String (Index);
      Is_Left_Align : Boolean := False;
      CS_Index      : Integer := 2;
      Number_First  : Integer;
      String_Size   : Integer := -1;
   begin
      loop
         case CC is
            when '%' =>
               Is_Left_Align := False;
               String_Size := -1;
               Index := Index + 1;
               CC := Format_String (Index);

               if CC = '-' then
                  Is_Left_Align := True;
                  Index := Index + 1;
                  CC := Format_String (Index);
               end if;

               case CC is
                  when '0' .. '9' =>
                     Number_First := Index;
                     while CC >= '0' and then CC <= '9' loop
                        Index := Index + 1;
                        CC := Format_String (Index);
                     end loop;
                     String_Size := Integer'Value
                       (Format_String (Number_First .. Index - 1));
                  when others => null;
               end case;

               case CC is
                  when 's' =>
                     if CS_Index > Length (Args) then
                        if String_Size > 0 then
                           declare
                              Result : constant String (1 .. String_Size) :=
                                (others => ' ');
                           begin
                              Append (Output, Result);
                           end;
                        end if;
                     else
                        declare
                           Arg : constant String := Element (Args, CS_Index);
                        begin

                           if Arg'Length >= String_Size then
                              Append (Output, Arg);
                           else
                              declare
                                 Result : String (1 .. String_Size) :=
                                   (others => ' ');
                              begin
                                 if Is_Left_Align then
                                    Result (1 .. Arg'Length) := Arg;
                                 else
                                    Result
                                      (String_Size - Arg'Length + 1 ..
                                         String_Size) := Arg;
                                 end if;
                                 Append (Output, Result);
                              end;
                           end if;
                        end;
                     end if;
                     CS_Index := CS_Index + 1;
                  when others => Append (Output, CC);
               end case;

            when '\' =>
               Index := Index + 1;
               CC := Format_String (Index);
               case CC is
                  when 'n' => Append (Output, ASCII.LF);
                  when 'r' => Append (Output, ASCII.CR);
                  when others => Append (Output, CC);
               end case;
            when others => Append (Output, CC);
         end case;
         Index := Index + 1;
         exit when Index > Format_String'Last;
         CC := Format_String (Index);
      end loop;

      Put (S, 1, To_String (Output));
      return (RESULT_STD, 0);
   end Printf_Builtin;

end Sh.Builtins.Printf;
