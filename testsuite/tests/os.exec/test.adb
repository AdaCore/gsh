with Test_Assert;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with GNAT.Strings;

with OS.Exec; use OS.Exec;

function Test return Integer
is
   package A renames Test_Assert;
   package GS renames GNAT.Strings;
   package Env renames Ada.Environment_Variables;
   S : Ada.Strings.Unbounded.Unbounded_String;
   Arg : Argument_List := (new String'(Env.Value ("PYTHON_TEST_EXEC", "")),
                           new String'("-c"),
                           new String'("print 'hello'"));
   Status : Integer;
begin
   S := Blocking_Spawn (Arg, Status => Status);
   A.Assert (Status = 0);
   return A.Report;
end Test;
