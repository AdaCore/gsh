------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Wc                            --
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

with Sh.States.IO;      use Sh.States.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Sh.Builtins.Wc is

   ----------------
   -- Wc_Builtin --
   ----------------

   function Wc_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is

      Global_Line_Count : Integer := 0;
      Global_Word_Count : Integer := 0;
      Global_Byte_Count : Integer := 0;
      Show_Line_Count : Boolean := False;
      Show_Word_Count : Boolean := False;
      Show_Byte_Count : Boolean := False;
      File_List_Index : Integer := -1;
      Has_Option : Boolean := False;

      procedure Count (Content : String; Content_Name : String);
      --  Update counter taking into account the content passed as argument.

      -----------
      -- Count --
      -----------

      procedure Count (Content : String; Content_Name : String) is
         Line_Count : Integer := 0;
         Word_Count : Integer := 0;
         Byte_Count : Integer := 0;
         In_Word : Boolean := False;
      begin
         for Index in Content'Range loop
            case Content (Index) is
               when ' ' | ASCII.HT | ASCII.LF | ASCII.CR =>
                  if In_Word then
                     Word_Count := Word_Count + 1;
                     In_Word := False;
                  end if;
               when others =>
                  In_Word := True;
            end case;
            if Content (Index) = ASCII.LF then
               Line_Count := Line_Count + 1;
            end if;
            Byte_Count := Byte_Count + 1;
         end loop;
         if In_Word then
            Word_Count := Word_Count + 1;
         end if;

         if Show_Line_Count then
            Put (S, 1, Integer'Image (Line_Count));
         end if;
         if Show_Word_Count then
            Put (S, 1, Integer'Image (Word_Count));
         end if;

         if Show_Byte_Count then
            Put (S, 1, Integer'Image (Byte_Count));
         end if;

         if Content_Name'Length > 0 then
            Put (S, 1, " " & Content_Name);
         end if;

         Put (S, 1, ASCII.LF & "");

         Global_Byte_Count := Global_Byte_Count + Byte_Count;
         Global_Word_Count := Global_Word_Count + Word_Count;
         Global_Line_Count := Global_Line_Count + Line_Count;
      end Count;

   begin
      --  Parse arguments
      for Index in 1 .. Length (Args) loop
         declare
            Arg : constant String := Element (Args, Index);
         begin
            if Arg (Arg'First) = '-' then
               Has_Option := True;
               for Char_Index in Arg'First + 1 .. Arg'Last loop
                  case Arg (Char_Index) is
                     when 'w' => Show_Word_Count := True;
                     when 'c' => Show_Byte_Count := True;
                     when 'l' => Show_Line_Count := True;
                     when others =>
                        Put (S, 2,
                             "unexpected option " &
                               Arg (Char_Index) & ASCII.LF);
                        return (RESULT_STD, 1);
                  end case;
               end loop;
            else
               File_List_Index := Index;
               exit;
            end if;
         end;
      end loop;

      if not Has_Option then
         --  no option has been passed. the default is to show everything
         Show_Line_Count := True;
         Show_Word_Count := True;
         Show_Byte_Count := True;
      end if;

      --  At this stage we read data either from files passed as arguments or
      --  if no file has been passed from stdin
      if File_List_Index /= -1 then
         for File_Index in File_List_Index .. Length (Args) loop
            declare
               Fd : File_Descriptor;
               Fl : Long_Integer := 0;
               Buffer : String_Access := null;
               R : Integer;
               pragma Unreferenced (R);
            begin
               Fd := Open_Read (Resolve_Path (S, Element (Args, File_Index)),
                                Binary);
               if Fd < 0 then
                  Put (S, 2, "wc: " & Element (Args, File_Index) &
                       ": No such file or directory" & ASCII.LF);
               else
                  Fl := File_Length (Fd);
                  Buffer := new String (1 .. Integer (Fl));
                  R := Read (Fd, Buffer.all'Address, Buffer.all'Last);
                  Count (Buffer.all, Element (Args, File_Index));
                  Close (Fd);
                  Free (Buffer);
               end if;

            end;
         end loop;

         --  more than 2 files so print totals
         if Length (Args) - File_List_Index > 0 then
            if Show_Line_Count then
               Put (S, 1, Integer'Image (Global_Line_Count));
            end if;

            if Show_Word_Count then
               Put (S, 1, Integer'Image (Global_Word_Count));
            end if;

            if Show_Byte_Count then
               Put (S, 1, Integer'Image (Global_Byte_Count));
            end if;

            Put (S, 1, " total" & ASCII.LF);

         end if;

      else
         Count (Read (S, 0), "");
      end if;

      return (RESULT_STD, 0);
   end Wc_Builtin;

end Sh.Builtins.Wc;
