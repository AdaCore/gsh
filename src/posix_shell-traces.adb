------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2015, AdaCore                   --
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

package body Posix_Shell.Traces is

   ---------
   -- Log --
   ---------

   procedure Log (Logger : String; Msg : String) is
      Final : constant String := Logger & ":" & Msg & ASCII.LF;
      use OS.FS;
   begin
      if Enable_Traces then
         if Logger_Handler = OS.FS.Invalid_FD then
            Logger_Handler := OS.FS.Open ("/tmp/gsh.out", Append_Mode);
         end if;

         OS.FS.Write (Logger_Handler, Final);
      end if;
   end Log;

end Posix_Shell.Traces;
