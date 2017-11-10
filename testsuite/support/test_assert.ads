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

--  Helper package to implement tests that comply with the expectations
--  of the default test driver.

with GNAT.Source_Info;

package Test_Assert is

   package SI renames GNAT.Source_Info;

   Final_Status : Natural := 0;

   procedure Assert
      (Success  : Boolean;
       Msg      : String := "";
       Location : String := SI.Source_Location);
   --  If Success is True then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert
      (Left, Right : String;
       Msg         : String := "";
       Location    : String := SI.Source_Location);
   --  If Left = Right then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   function Report return Natural;
   --  Report should be called the following way at the end of a test
   --  program main function:
   --
   --      return Report;
   --
   --  Testsuite driver will consider a test to PASS if all the
   --  following conditions are met:
   --
   --  * test program exit with status 0
   --  * all assert calls did succeed
   --  * test program display the message "<=== TEST PASSED ===>"
end Test_Assert;
