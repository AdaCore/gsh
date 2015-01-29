------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Echo                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2014, AdaCore                   --
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

with Posix_Shell.String_Utils;          use Posix_Shell.String_Utils;
with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Echo is

   ------------------
   -- Echo_Builtin --
   ------------------

   function Echo_Builtin
     (S : in out Shell_State;
      Args : String_List)
      return Integer
   is
      Enable_Newline : Boolean := True;
      Enable_Backslash : Boolean := False;
      In_Options : Boolean := True;

      function Transform_Backslashes (Input : String) return String;

      function Transform_Backslashes (Input : String) return String
      is
         Result : String (1 .. Input'Length);
         Index : Integer := Input'First;
         Result_Last : Integer := 0;
      begin
         while Index <= Input'Last loop
            case Input (Index) is
               when '\' =>
                  Index := Index + 1;
                  if Index > Input'Last then
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '\';
                     exit;
                  end if;
                  case Input (Index) is
                     when '\' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := '\';
                     when 'a' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.BEL;
                     when 'b' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.DEL;
                     when 'e' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.ESC;
                     when 'f' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.FF;
                     when 'n' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.LF;
                     when 'r' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.CR;
                     when 't' =>
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := ASCII.HT;
                     when others =>
                        Result_Last := Result_Last + 2;
                        Result (Result_Last - 1) := Input (Index - 1);
                        Result (Result_Last) := Input (Index);
                  end case;
               when others =>
                  Result_Last := Result_Last + 1;
                  Result (Result_Last) := Input (Index);
            end case;
            Index := Index + 1;
         end loop;
         return Result (1 .. Result_Last);
      end Transform_Backslashes;

   begin
      for I in Args'Range loop
         if Args (I).all /= "" then

            case Args (I).all (Args (I)'First) is
               when '-' =>
                  if Args (I).all = "-n" and In_Options then
                     Enable_Newline := False;
                  elsif Args (I).all = "-e" and In_Options then
                     Enable_Backslash := True;
                  else
                     In_Options := False;
                     if Enable_Backslash then
                        Put (S, 1, Transform_Backslashes (Args (I).all));
                     else
                        Put (S, 1, Args (I).all);
                     end if;
                     if I < Args'Last then
                        Put (S, 1, " ");
                     end if;
                  end if;
               when others =>
                  In_Options := False;
                  if Enable_Backslash then
                     Put (S, 1, Transform_Backslashes (Args (I).all));
                  else
                     Put (S, 1, Args (I).all);
                  end if;
                  if I < Args'Last then
                     Put (S, 1, " ");
                  end if;
            end case;
         end if;
      end loop;

      if Enable_Newline then
         New_Line (S, 1);
      end if;
      return 0;
   end Echo_Builtin;

   -------------------
   -- REcho_Builtin --
   -------------------

   function REcho_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      function Replace_LF (S : String) return String;

      function Replace_LF (S : String) return String is
         Result : String (S'First .. S'Last * 2);
         Result_Index : Integer := Result'First;
      begin
         for I in S'Range loop
            if S (I) = ASCII.LF then
               Result (Result_Index) := '^';
               Result (Result_Index + 1) := 'J';
               Result_Index := Result_Index + 2;
            else
               Result (Result_Index) := S (I);
               Result_Index := Result_Index + 1;
            end if;
         end loop;
         return Result (S'First .. Result_Index - 1);
      end Replace_LF;
   begin
      for I in Args'Range loop
         Put (S, 1, "argv[" & To_String (I - Args'First + 1) & "] = <" &
              Replace_LF (Args (I).all) & ">");
         if I < Args'Last then
            New_Line (S, 1);
         end if;
      end loop;
      New_Line (S, 1);
      return 0;
   end REcho_Builtin;

end Posix_Shell.Builtins.Echo;
