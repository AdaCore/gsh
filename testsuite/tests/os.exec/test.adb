with Test_Assert;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with GNAT.Strings;
with C.Strings; use C.Strings;
with OS.Exec; use OS.Exec;

function Test return Integer
is
   package A renames Test_Assert;
   package GS renames GNAT.Strings;
   package EnvVar renames Ada.Environment_Variables;
   S : Ada.Strings.Unbounded.Unbounded_String;
   Arg : CList;
   Status : Integer;
   Env    : CList;
   begin
   C.Strings.Append(Arg, EnvVar.Value ("PYTHON_TEST_EXEC", ""));
   C.Strings.Append(Arg, "-c");
   C.Strings.Append(Arg, "print('hello')");
   S := Blocking_Spawn (Arg, Env => Env, Status => Status);
   A.Assert (Status = 0);
   return A.Report;
end Test;
