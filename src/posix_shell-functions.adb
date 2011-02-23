with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;

package body Posix_Shell.Functions is

   package Function_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String,
         Element_Type => Shell_Tree_Access,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   use Function_Maps;

   Function_Map : Function_Maps.Map;
   --  A table that maps function names to their associated Node.

   ----------------------
   -- Execute_Function --
   ----------------------

   procedure Execute_Function
     (State : Shell_State_Access;
      Name  : String;
      Args : String_List)
   is
      Function_Tree : constant Shell_Tree_Access :=
        Element (Function_Map, Name);
      Saved_Pos_Params : constant Pos_Params_State :=
        Get_Positional_Parameters (State.all);

   begin
      Set_Positional_Parameters (State.all, Args, False);
      pragma Assert (Function_Tree /= null);

      Eval (State, Function_Tree.all);
      Restore_Positional_Parameters (State.all, Saved_Pos_Params);

   exception
      when Shell_Exit_Exception =>
         Restore_Positional_Parameters (State.all, Saved_Pos_Params);
         raise;
   end Execute_Function;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Name : String) return Boolean is
   begin
      return Contains (Function_Map, Name);
   end Is_Function;

   -----------------------
   -- Register_Function --
   -----------------------

   procedure Register_Function (Name : String; Tree : Shell_Tree_Access) is
   begin
      Include (Function_Map, Name, Tree);
   end Register_Function;

end Posix_Shell.Functions;
