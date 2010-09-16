package Posix_Shell.Tree.Evals is

   Continue_Exception : exception;
   Break_Exception : exception;

   function Eval (N : Node_Id) return Integer;
   function Eval (N : Node_Id) return String;

   function Eval (N : Node) return Integer;
end Posix_Shell.Tree.Evals;
