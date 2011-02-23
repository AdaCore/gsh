with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Tree.Evals is

   Continue_Exception : exception;
   Break_Exception : exception;

   procedure Eval (S : Shell_State_Access; T : Shell_Tree);
   procedure Eval (S : Shell_State_Access; T : Shell_Tree; N : Node_Id);
   procedure Eval (S : Shell_State_Access; T : Shell_Tree; N : Node);

   function Eval
     (S : Shell_State_Access;
      T : Shell_Tree) return String;

end Posix_Shell.Tree.Evals;
