with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package body Posix_Shell.Functions is

   package Function_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String,
         Element_Type => Node_Access,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   use Function_Maps;

   Function_Map : Function_Maps.Map;
   --  A table that maps function names to their associated Node.

   ----------------------
   -- Execute_Function --
   ----------------------

   function Execute_Function (Name : String; Args : String_List)
     return Integer
   is
      Exit_Value : Integer;
      Node : constant Node_Access := Element (Function_Map, Name);
      Saved_Pos_Params : constant Pos_Params_State :=
        Set_Positional_Parameters (Args);
   begin
      pragma Assert (Node /= null);

      Exit_Value := Eval (Node.all);
      Restore_Positional_Parameters (Saved_Pos_Params);

      return Exit_Value;

   exception
      when Shell_Exit_Exception =>
         Restore_Positional_Parameters (Saved_Pos_Params);
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

   procedure Register_Function (Name : String; Node : Node_Access) is
   begin
      Include (Function_Map, Name, Node);
   end Register_Function;

end Posix_Shell.Functions;
