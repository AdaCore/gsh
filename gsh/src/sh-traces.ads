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

with OS.FS;
with GNAT.Strings; use GNAT.Strings;

package Sh.Traces is

   type Trace_Channel is
     (LOG_LEXER,
      LOG_PARSER,
      LOG_SUBST,
      LOG_RE,
      LOG_EVAL,
      LOG_BUILTIN,
      LOG_EXEC);
   --  List of available logging channels

   Channel_Status : array (Trace_Channel'Range) of Boolean :=
     (others => False);
   --  Table that maintain channel status. If True messages on the channel are
   --  enabled.

   Logger_Handler : OS.FS.File_Descriptor := OS.FS.Invalid_FD;
   --  Target of log messages. Default is to use stderr

   procedure Log (Channel : Trace_Channel; Msg : String);
   --  Emit log message on a given channel
   --
   --  @param Channel logging channel on which the message is sent
   --  @param Msg message content

   procedure Log (Channel : Trace_Channel; Msg_List : String_List);

end Sh.Traces;
