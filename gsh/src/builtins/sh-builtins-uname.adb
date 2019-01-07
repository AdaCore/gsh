------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Uname                         --
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

package body Sh.Builtins.Uname is

   -------------------
   -- Uname_Builtin --
   -------------------

   function Uname_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      Kernel_Name       : Boolean := False;
      Node_Name         : Boolean := False;
      Kernel_Release    : Boolean := False;
      Kernel_Version    : Boolean := False;
      Machine           : Boolean := False;
      Processor         : Boolean := False;
      Hardware_Platform : Boolean := False;
      Operating_System  : Boolean := False;
      Add_Space         : Boolean := False;

   begin
      if Length (Args) = 0 then
         Kernel_Name := True;

      else
         for Index in 1 .. Length (Args) loop
            declare
               Arg : constant String := Element (Args, Index);
            begin
               case Arg (Arg'First) is
                  when '-' =>
                     if Arg = "--help" then
                        Put (S, 1, "usage: uname [OPTIONS]" & ASCII.LF);
                        return (RESULT_STD, 0);

                     elsif Arg = "--" then
                           Put (S, 2, "uname: unexpected operand" & ASCII.LF);
                           return (RESULT_STD, 1);
                     else
                        for Index2 in Arg'First + 1 .. Arg'Last loop
                           case Arg (Index2) is
                              when 'a' =>
                                 Kernel_Name := True;
                                 Node_Name := True;
                                 Kernel_Release := True;
                                 Kernel_Version := True;
                                 Machine := True;
                                 Operating_System := True;
                              when 's' => Kernel_Name := True;
                              when 'n' => Node_Name := True;
                              when 'r' => Kernel_Release := True;
                              when 'v' => Kernel_Version := True;
                              when 'm' => Machine := True;
                              when 'p' => Processor := True;
                              when 'i' => Hardware_Platform := True;
                              when 'o' => Operating_System := True;
                              when others =>
                                 Put (S, 2,
                                      "uname: unknown option" & ASCII.LF);
                                 return (RESULT_STD, 1);
                           end case;
                        end loop;
                     end if;

                  when others =>
                     Put (S, 2, "uname: extra operand" & ASCII.LF);
                     return (RESULT_STD, 1);
               end case;
            end;
         end loop;
      end if;

      if Kernel_Name then
         Put (S, 1, "CYGWIN_NT-6.0-WOW64");
         Add_Space := True;
      end if;

      if Node_Name then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "machine");
         Add_Space := True;
      end if;

      if Kernel_Release then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "1.7.0(0.212/5/3)");
         Add_Space := True;
      end if;

      if Kernel_Version then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "2009-09-11 01:25");
         Add_Space := True;
      end if;

      if Machine then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "i686");
         Add_Space := True;
      end if;

      if Processor then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "unknown");
         Add_Space := True;
      end if;

      if Hardware_Platform then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "unknown");
         Add_Space := True;
      end if;

      if Operating_System then
         if Add_Space then
            Put (S, 1, " ");
         end if;
         Put (S, 1, "Cygwin");
      end if;

      Put (S, 1, "" & ASCII.LF);

      return (RESULT_STD, 0);
   end Uname_Builtin;

end Sh.Builtins.Uname;
