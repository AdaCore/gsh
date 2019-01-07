------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Basename                      --
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

with Sh.States.IO; use Sh.States.IO;

package body Sh.Builtins.Basename is

   ----------------------
   -- Basename_Builtin --
   ----------------------

   function Basename_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      First : Integer := 0;
   begin
      if Length (Args) = 0 then
         Error (S, "basename: need at least one operand");
         return (RESULT_STD, 1);
      end if;

      if Element (Args, 1) = "--" then
         First := 2;
      else
         First := 1;
      end if;

      if Length (Args) - First + 1 > 2 then
         Error (S, "basename: does not accept more than two operands");
         return (RESULT_STD, 1);
      end if;

      if Element (Args, First) = "" then
         Put (S, 1, "" & ASCII.LF);
         return (RESULT_STD, 0);
      end if;

      if Element (Args, First) = "//" or else Element (Args, First) = "\\" then
         Put (S, 1, Element (Args, First) & ASCII.LF);
         return (RESULT_STD, 0);
      end if;

      declare
         Path : constant String := Element (Args, First);
         Filename_Start : Integer := Path'First;
         Filename_End : Integer := Path'Last;
      begin
         while Path (Filename_End) = '/' or else Path (Filename_End) = '\' loop
            Filename_End := Filename_End - 1;
            if Filename_End < Filename_Start then
               --  Only slashes and backslashes in the path so return a /
               Put (S, 1, "/" & ASCII.LF);
               return (RESULT_STD, 0);
            end if;
         end loop;

         for Index in reverse Path'First .. Filename_End loop
            if Path (Index) = '/' or Path (Index) = '\' then
               Filename_Start := Index + 1;
               exit;
            end if;
         end loop;

         if Length (Args) - First + 1 = 2 then
            declare
               Ext : constant String := Element (Args, First + 1);
            begin
               if Filename_End - Filename_Start + 1 >= Ext'Length and then
                 Path (Filename_End - Ext'Length + 1 .. Filename_End) = Ext
               then
                  Put (S, 1,
                       Path (Filename_Start .. Filename_End - Ext'Length)
                       & ASCII.LF);
                  return (RESULT_STD, 0);
               end if;
            end;
         end if;

         Put (S, 1, Path (Filename_Start .. Filename_End) & ASCII.LF);
         return (RESULT_STD, 0);
      end;
   end Basename_Builtin;

end Sh.Builtins.Basename;
