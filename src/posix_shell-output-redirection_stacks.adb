------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2014, AdaCore                   --
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

separate (Posix_Shell.Output)

package body Redirection_Stacks is

   Stack : array (1 .. 128) of Shell_Descriptors
     := (others =>
           ((Standin,  null, False),
            (Standout, null, False),
            (Standin,  null, False),
            (Standout, null, False),
            (Standerr, null, False),
            others => (Invalid_FD, null, False)));
   --  Stack that contains the redirection state for the different contexts.
   --  ??? Lift this hard-coded limitation one day. Should be pretty easy:
   --  Replace this by an access to unbounded array, and re-allocate
   --  during a Push if necessary.  Better yet, use one of the arrays
   --  either in the GNAT or the Ada hierarchy.

   Stack_Index : Positive := 1;
   --  The index of the top element of our stack.

   ------------------------
   -- Get_Current_States --
   ------------------------

   function Get_Current_States return Shell_Descriptors is
   begin
      return Stack (Stack_Index);
   end Get_Current_States;

   ---------
   -- Pop --
   ---------

   procedure Pop is
   begin
      pragma Assert (Stack_Index > 1);
      Stack_Index := Stack_Index - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (R : Shell_Descriptors) is
   begin
      Stack_Index := Stack_Index + 1;
      Stack (Stack_Index) := R;
   end Push;

   procedure Set (R : Shell_Descriptors) is
   begin
      Stack (Stack_Index) := R;
   end Set;

end Redirection_Stacks;
