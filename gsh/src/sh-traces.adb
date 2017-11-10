------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

with GNAT.Formatted_String; use GNAT.Formatted_String;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables; use Ada.Environment_Variables;
use type Ada.Streams.Stream_Element_Count;

package body Sh.Traces is

   HTTP_Trace_Port : constant String := Value ("GSH_HTTP_DEBUG", "");

   ---------
   -- Log --
   ---------

   procedure Log (Channel : Trace_Channel; Msg : String) is
      Format_Str : Formatted_String := +"[%-16s] %s";
      Channel_Img : constant String := Channel'Img;
      use OS.FS;
   begin

      if Channel_Status (Channel) then
         if Logger_Handler = OS.FS.Invalid_FD then
            Logger_Handler := OS.FS.Standerr;
         end if;
         Format_Str := Format_Str & Channel'Img (5 .. Channel_Img'Last) & Msg;

         OS.FS.Write (Logger_Handler, -Format_Str);
         OS.FS.Write (Logger_Handler, "" & ASCII.LF);
      end if;

      if HTTP_Trace_Port /= "" and then Channel = LOG_EXEC then
         declare
            Address : Sock_Addr_Type;
            Socket  : Socket_Type;
            Channel : Stream_Access;
            Offset  : Ada.Streams.Stream_Element_Count;
            Data    : Ada.Streams.Stream_Element_Array (1 .. 1024);
         begin

            Address.Addr := Inet_Addr ("127.0.0.1");
            Address.Port := 5555;
            Create_Socket (Socket);

            --  Set_Socket_Option (Socket,
            --                   Socket_Level,
            --                    (Reuse_Address, True));
            Connect_Socket (Socket, Address);
            Channel := Stream (Socket);
            String'Write (Channel,
                           "POST /debug HTTP/1.0" & ASCII.CR & ASCII.LF);
            String'Write (Channel,
                           "Content-Type: text/plain" & ASCII.LF);
            String'Write
              (Channel,
               "Content-Length:" & Msg'Length'Img & ASCII.LF);
            String'Write (Channel,
                           "" & ASCII.LF);
            String'Write (Channel, Msg &  ASCII.LF & ASCII.LF);

            loop
               Ada.Streams.Read (Channel.all, Data, Offset);
               exit when Offset = 0;
            end loop;

            Close_Socket (Socket);

         exception
            when others =>
               OS.FS.Write (Logger_Handler, "error connecting" & ASCII.LF);

         end;
      end if;
   end Log;

   procedure Log (Channel : Trace_Channel; Msg_List : String_List)
   is
      Msg : Unbounded_String := To_Unbounded_String ("");
   begin
      for Index in Msg_List'Range loop
         Append (Msg, Msg_List (Index).all);
         Append (Msg, " ");
      end loop;

      Log (Channel, To_String (Msg));
   end Log;

end Sh.Traces;
