------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Dirname                       --
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

package body Sh.Builtins.Dirname is

   ----------------------
   -- Dirname_Builtin --
   ----------------------

   function Dirname_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      First : Integer := 0;
   begin
      if Length (Args) = 0 then
         Error (S, "dirname: need at least one operand");
         return (RESULT_STD, 1);
      end if;

      if Element (Args, 1) = "--" then
         First := 2;
      else
         First := 1;
      end if;

      if Length (Args) - First + 1 > 1 then
         Error (S, "dirname: does not accept more than one operand");
         return (RESULT_STD, 1);
      end if;

      if Element (Args, First) = "" then
         Put (S, 1, "." & ASCII.LF);
         return (RESULT_STD, 0);
      end if;

      --  Step 1. If string is "//" just preserved the // which has special
      --  meaning on windows.
      if Element (Args, First) = "//" or else Element (Args, First) = "\\" then
         Put (S, 1, Element (Args, First) & ASCII.LF);
         return (RESULT_STD, 0);
      end if;

      declare
         Path : constant String := Element (Args, First);
         Filename_End : Integer := Path'Last;
      begin
         --  Remove trailing slashes (step 2 and 3)
         while Path (Filename_End) = '/' or else Path (Filename_End) = '\' loop
            Filename_End := Filename_End - 1;
            if Filename_End < Path'First then
               --  Only slashes and backslashes in the path so return a /
               Put (S, 1, "/" & ASCII.LF);
               return (RESULT_STD, 0);
            end if;
         end loop;

         --  Remove trailing non slash characters (step 5)
         for Index in reverse Path'First .. Filename_End loop
            Filename_End := Index - 1;
            if Path (Index) = '/' or Path (Index) = '\' then
               Filename_End := Index;
               exit;
            end if;
         end loop;

         --  No slashes remain in the string so return .
         if Filename_End < Path'First then
            --  no slashes left in path so return "."
            Put (S, 1, "." & ASCII.LF);
            return (RESULT_STD, 0);
         end if;

         --  check if we have //
         if Path (Path'First .. Filename_End) = "//" or else
           Path (Path'First .. Filename_End) = "\\"
         then
            Put (S, 1, Path (Path'First .. Filename_End) & ASCII.LF);
            return (RESULT_STD, 0);
         end if;

         --  Remove trailing slashes
         for Index in reverse Path'First .. Filename_End loop
            if Path (Index) /= '/' and then Path (Index) /= '\' then
               Filename_End := Index;
               exit;
            else
               Filename_End := Index - 1;
            end if;
         end loop;

         if Filename_End < Path'First then
            Put (S, 1, "/" & ASCII.LF);
            return (RESULT_STD, 0);
         end if;

         Put (S, 1, Path (Path'First .. Filename_End) & ASCII.LF);
         return (RESULT_STD, 0);
      end;
   end Dirname_Builtin;

end Sh.Builtins.Dirname;
