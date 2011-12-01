with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins_Printf is

   function Builtin_Printf
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Format_String : constant String := Args (Args'First).all;
      Output : Unbounded_String := To_Unbounded_String ("");
      Index  : Integer := Format_String'First;
      CC     : Character := Format_String (Index);
      Is_Left_Align : Boolean := False;
      CS_Index : Integer := Args'First + 1;
      Number_First : Integer;
      String_Size : Integer := -1;
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
                     if CS_Index > Args'Last then
                        if String_Size > 0 then
                           declare
                              Result : constant String (1 .. String_Size) :=
                                (others => ' ');
                           begin
                              Append (Output, Result);
                           end;
                        end if;
                     else
                        if Args (CS_Index)'Length >= String_Size then
                           Append (Output, Args (CS_Index).all);
                        else
                           declare
                              Result : String (1 .. String_Size) :=
                                (others => ' ');
                           begin
                              if Is_Left_Align then
                                 Result (1 .. Args (CS_Index)'Length) :=
                                   Args (CS_Index).all;
                              else
                                 Result
                                   (String_Size - Args (CS_Index)'Length + 1 ..
                                      String_Size) := Args (CS_Index).all;
                              end if;
                              Append (Output, Result);
                           end;
                        end if;
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

      Put (S.all, 1, To_String (Output));
      return 0;
   end Builtin_Printf;

end Posix_Shell.Builtins_Printf;
