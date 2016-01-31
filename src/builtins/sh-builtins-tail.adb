------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Tail                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with Sh.States.Output; use Sh.States.Output;
with Sh.Utils; use Sh.Utils;
with Sh.String_Utils; use Sh.String_Utils;

package body Sh.Builtins.Tail is

   ------------------
   -- Tail_Builtin --
   ------------------

   function Tail_Builtin
     (S : in out Shell_State;
      Args : String_List)
      return Eval_Result
   is
      Buffer        : String_Access := null;
      Current_Arg   : Integer := Args'First;
      Args_Last     : constant Integer := Args'Last;
      Use_Char_Mode : Boolean := False;
      --  tail can either count lines or characters. Default mode is line mode.

      Relative_To_End : Boolean := True;
      Line_Number   : Integer := 10;

      Enable_Minus_N_Opt : Boolean := True;

      function Parse_Number (S : String) return Boolean;
      --  return true if the argument is +n, -n or n where n a natural

      function Parse_Number (S : String) return Boolean
      is
         Is_Valid : Boolean;
      begin
         if Starts_With (S, "+") then
            To_Integer (S (S'First + 1 .. S'Last), Line_Number, Is_Valid);
            Relative_To_End := False;
         elsif Starts_With (S, "-") then
            To_Integer (S (S'First + 1 .. S'Last), Line_Number, Is_Valid);
         else
            To_Integer (S, Line_Number, Is_Valid);
         end if;
         return Is_Valid;
      end Parse_Number;

   begin
      --  First parse options
      while Current_Arg <= Args_Last loop
         declare
            CA : constant String := Args (Current_Arg).all;
         begin

            if CA = "-f" then
               --  ignore that option for the moment
               null;
            elsif CA = "-c" or else CA = "-n" then
               --  We cannot have a -<number> option after a -n
               Enable_Minus_N_Opt := False;

               if CA = "-c" then
                  Use_Char_Mode := True;
               end if;
               Current_Arg := Current_Arg + 1;

               --  Check that we have an argument
               if Current_Arg > Args_Last then
                  Error (S, "tail: option -c requires an argument");
                  return (RESULT_STD, 1);
               end if;

               --  Check that the argument is an integer
               if not Parse_Number (Args (Current_Arg).all) then
                  Error (S, "tail: invalid context");
                  return (RESULT_STD, 1);
               end if;

            elsif Starts_With (CA, "-") then
               --  Check that the argument is an integer
               declare
                  Is_Valid : Boolean;
               begin
                  To_Integer (CA (CA'First + 1 .. CA'Last),
                              Line_Number, Is_Valid);
                  if not Is_Valid or else not Enable_Minus_N_Opt then
                     Error (S, "tail: invalid context");
                     return (RESULT_STD, 1);
                  end if;

                  --  Only one -number option is accepted
                  Enable_Minus_N_Opt := False;
               end;
            else
               exit;
            end if;

            Current_Arg := Current_Arg + 1;
         end;

      end loop;

      if Current_Arg > Args'Last then
         Buffer := new String'(Read (S, 0));
      else
         declare
            Fd : File_Descriptor;
            Byte_Reads : Integer;
            Length : Long_Integer;
         begin
            Fd := Open_Read (Resolve_Path (S, Args (Current_Arg).all),
                             Binary);
            Length := File_Length (Fd);
            Buffer := new String (1 .. Integer (Length));
            Byte_Reads := Read (Fd, Buffer.all'Address, Buffer.all'Last);
            Close (Fd);
            if Byte_Reads /= Integer (Length) then
               Put (S, 2, "tail: cannot read file");
               return (RESULT_STD, 1);
            end if;
         end;
      end if;

      if Use_Char_Mode then
         if Relative_To_End then
            null;
         else
            null;
         end if;

      else
         if Relative_To_End then
            Put (S, 1, Last_Lines (Buffer.all, Line_Number));
         else
            Put (S, 1, From_Line (Buffer.all, Line_Number));
         end if;

      end if;
      Free (Buffer);
      return (RESULT_STD, 0);
   end Tail_Builtin;

end Sh.Builtins.Tail;
