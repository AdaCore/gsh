separate (Posix_Shell.Output)

package body Redirection_Stacks is

   Stack : array (1 .. 128) of Redirection_States
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

   function Get_Current_States return Redirection_States is
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

   procedure Push (R : Redirection_States) is
   begin
      Stack_Index := Stack_Index + 1;
      Stack (Stack_Index) := R;
   end Push;

   procedure Set (R : Redirection_States) is
   begin
      Stack (Stack_Index) := R;
   end Set;

end Redirection_Stacks;
