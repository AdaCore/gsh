------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

with GNAT.Formatted_String; use GNAT.Formatted_String;

package body Sh.Traces is

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
   end Log;

end Sh.Traces;
