------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

package body Test_Assert is
   package IO renames Ada.Text_IO;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Success  : Boolean;
       Msg      : String := "";
       Location : String := SI.Source_Location)
   is
   begin
      IO.Put (Location & ": ");
      if Success then
         IO.Put ("PASSED:");
      else
         IO.Put ("FAILED:");
         Final_Status := 1;
      end if;
      if Msg'Length > 0 then
         IO.Put (" ");
         IO.Put (Msg);
      end if;
      IO.New_Line;
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Left, Right : String;
       Msg         : String := "";
       Location    : String := SI.Source_Location)
   is
      Success : constant Boolean := Left = Right;
   begin
      Assert (Success, Msg, Location);
      if not Success then
         if Right'Length > 0 then
            IO.Put_Line ("expected: " & Right);
         else
            IO.Put_Line ("expected empty string");
         end if;

         if Left'Length > 0 then
            IO.Put_Line ("got:      " & Left);
         else
            IO.Put_Line ("got empty string");
         end if;
      end if;
   end Assert;

   ------------
   -- Report --
   ------------

   function Report return Natural is
   begin
      if Final_Status = 0 then
         IO.Put_Line ("<=== TEST PASSED ===>");
      else
         IO.PUT_Line ("<=== TEST FAILED ===>");
      end if;
      return Final_Status;
   end Report;

end Test_Assert;
