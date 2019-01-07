------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Cat                           --
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

with Sh.String_Utils;     use Sh.String_Utils;
with Sh.States.IO; use Sh.States.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Sh.Builtins.Cat is

   -----------------
   -- Cat_Builtin --
   -----------------

   function Cat_Builtin
     (S : in out Shell_State;
      Args : CList)
      return Eval_Result
   is
      Fd     : File_Descriptor;
      Buffer : aliased String (1 .. 8192);
      R      : Integer;
   begin

      --  If no argument is given to cat then we assume that stdin should be
      --  dump.
      if Length (Args) = 0 then
         Put (S, 1, Read (S, 0));
         return (RESULT_STD, 0);
      end if;

      for J in 1 .. Length (Args) loop
         --  '-' means that we need to dump stdin otherwise the argument is
         --  interpreted as a filename.
         if Element (Args, J) = "-" then
            Put (S, 1, Read (S, 0));
         else
            if GNAT.OS_Lib.Is_Directory (Element (Args, J)) then
               Put (S, 2, "cat: " & Element (Args, J) &
                      ": Is a directory" & ASCII.LF);
            else
               Fd := Open_Read (Resolve_Path (S, Element (Args, J)), Binary);
               if Fd < 0 then
                  Put (S, 2, "cat: " & Element (Args, J) &
                         ": No such file or directory" & ASCII.LF);
               else
                  loop
                     R := Read (Fd, Buffer'Address, Buffer'Last);
                     if R > 0 then
                        Put (S, 1, Strip_CR (Buffer (1 .. R)));
                     end if;
                     exit when R /= Buffer'Last;
                  end loop;
                  Close (Fd);
               end if;
            end if;
         end if;
      end loop;

      return (RESULT_STD, 0);
   end Cat_Builtin;

end Sh.Builtins.Cat;
