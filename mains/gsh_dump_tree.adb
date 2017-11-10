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

with Sh.Parser; use Sh.Parser;
with Sh.Tree; use Sh.Tree;
with Sh.Tree.Dumps; use Sh.Tree.Dumps;
with Ada.Command_Line; use Ada.Command_Line;
with Sh; use Sh;

-------------------
-- GSH_Dump_Tree --
-------------------

function GSH_Dump_Tree return Integer
is
   Status : constant Integer := 0;
   Tree   : constant Shell_Tree := Parse_File (Argument (1));
begin
   Dump (Tree);
   return Status;
end GSH_Dump_Tree;
