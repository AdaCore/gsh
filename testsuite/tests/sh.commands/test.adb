with Test_Assert;
with Sh.Commands;
with Ada.Environment_Variables;
with Sh;
with Sh.States;
with C.Strings; use C.Strings;

function Test return Integer is
   package A renames Test_Assert;
   package EnvVar renames Ada.Environment_Variables;
   Args   : CList;
   Result : Sh.Eval_Result;
   S      : Sh.States.Shell_State;
   use type Sh.Result_Kind;
   Env    : CList;
begin
   Append (Args, EnvVar.Value ("PYTHON_TEST_EXEC", ""));
   Append (Args, "-c");
   Append (Args, "print('Hello from Python')");
   Sh.States.Import_Environment (S);
   Sh.States.Get_Environment (S, Env);
   Result := Sh.Commands.Run (S, Element (Args, 1), Args, Env);
   A.Assert (Result.Kind = Sh.RESULT_STD and then Result.Status = 0, "run python command");

   Deallocate (Args);
   Append (Args, "./test1.py");
   Result := Sh.Commands.Run (S, Element (Args, 1), Args, Env);
   A.Assert (Result.Kind = Sh.RESULT_STD and then Result.Status = 0, "run python command");
   return A.Report;
end Test;
