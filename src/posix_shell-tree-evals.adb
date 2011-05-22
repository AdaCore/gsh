with Posix_Shell.Regexp; use Posix_Shell.Regexp;

with Posix_Shell.Commands_Preprocessor; use Posix_Shell.Commands_Preprocessor;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Functions; use Posix_Shell.Functions;
with Posix_Shell.Subst; use Posix_Shell.Subst;
with Posix_Shell.Utils; use  Posix_Shell.Utils;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Task_Lock;

package body Posix_Shell.Tree.Evals is

   procedure Eval_List (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_Case (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_If (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_And_Or_List
     (S : Shell_State_Access; T : Shell_Tree; N : Node);

   procedure Eval_Until_While
     (S : Shell_State_Access; T : Shell_Tree; N : Node; Is_Until : Boolean);
   procedure Eval_Pipe (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_For (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_Subshell (S : Shell_State_Access; T : Shell_Tree; N : Node);
   procedure Eval_Function (S : Shell_State_Access; N : Node);
   procedure Eval_Brace (S : Shell_State_Access; T : Shell_Tree; N : Node);

   procedure Eval_Cmd (S            : Shell_State_Access;
                       T            : Shell_Tree;
                       Command      : String;
                       Arguments    : String_List;
                       Redirections : Redirection_Op_Stack);
   --  Evaluate the call to the given Command with the given Arguments.
   --  This function takes care of setting and the restoring the redirections
   --  as well.

   procedure Eval_Cmd (S : Shell_State_Access; T : Shell_Tree; N : Node);
   --  Evaluate the given node as a command, after having expanded
   --  the command name and its arguments.

   procedure Eval_Assign
     (S : Shell_State_Access; N : Node;
      Do_Export : Boolean := False);

   ----------
   -- Eval --
   ----------

   procedure Eval (S : Shell_State_Access; T : Shell_Tree) is
   begin
      Eval (S, T, T.Toplevel_Node);
   end Eval;

   ----------
   -- Eval --
   ----------

   procedure Eval
     (S : Shell_State_Access;
      T : Shell_Tree;
      N : Node_Id)
   is
   begin
      if N = 0 then
         return;
      else
         Eval (S, T, T.Node_Table.Table (N).all);
      end if;
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (S : Shell_State_Access;
      T : Shell_Tree) return String
   is

      Status : Integer;
      pragma Warnings (Off, Status);

      Read_Fd : File_Descriptor;

      task Eval_Task is
         entry Start (Pipe_Input : out File_Descriptor);
         entry Get_Exit_Status;
      end Eval_Task;


      task body Eval_Task is
         S_Copy : constant Shell_State_Access := new Shell_State;
      begin
         accept Start (Pipe_Input : out File_Descriptor) do
            --  Create a new scope and create the pipe. The task is in charge
            --  of closing the writable side of the pipe and the main task the
            --  reading part
            GNAT.Task_Lock.Lock;
            S_Copy.all := Enter_Scope (S.all);
            Set_Pipe_Out (S_Copy.all);

            --  Pass to the main task the fd to the readble side of the pipe.
            Pipe_Input := Get_Fd (S_Copy.all, -2);
            GNAT.Task_Lock.Unlock;
         end Start;

         Eval (S_Copy, T);
         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe).
         Close (Get_Fd (S_Copy.all, 1));

         accept Get_Exit_Status do
            --  Destroy scope and ensure exit_status is correctly set.
            GNAT.Task_Lock.Lock;
            Leave_Scope (S_Copy.all, S.all);
            GNAT.Task_Lock.Unlock;
         end Get_Exit_Status;
      end Eval_Task;

   begin
      Eval_Task.Start (Read_Fd);
      declare
         Output : constant String := Read_Pipe_And_Close (S, Read_Fd);
      begin
         Eval_Task.Get_Exit_Status;
         return Strip_CR (Output);
      end;
   end Eval;

   ----------
   -- Eval --
   ----------

   procedure Eval (S : Shell_State_Access; T : Shell_Tree; N : Node) is
   begin
      case N.Kind is
         when LIST_NODE        => Eval_List (S, T, N);
         when CASE_NODE        => Eval_Case (S, T, N);
         when IF_NODE          => Eval_If (S, T, N);
         when AND_OR_LIST_NODE => Eval_And_Or_List (S, T, N);
         when WHILE_NODE       => Eval_Until_While (S, T, N, False);
         when UNTIL_NODE       => Eval_Until_While (S, T, N, True);
         when FOR_NODE         => Eval_For (S, T, N);
         when PIPE_NODE        => Eval_Pipe (S, T, N);
         when BRACE_NODE       => Eval_Brace (S, T, N);
         when SUBSHELL_NODE    => Eval_Subshell (S, T, N);
         when FUNCTION_NODE    => Eval_Function (S, N);
         when ASSIGN_NODE      => Eval_Assign (S, N);
         when CMD_NODE         => Eval_Cmd (S, T, N);
         when NULL_CMD_NODE    => null;
         when others           => raise Program_Error;
      end case;
      return;
   end Eval;

   -------------------
   -- Eval_And_List --
   -------------------

   procedure Eval_And_Or_List
     (S : Shell_State_Access; T : Shell_Tree; N : Node)
   is
      Go_To  : List_Kind := AND_LIST;
   begin
      for Index in N.And_Or_List_Childs'Range loop
         if Index = N.And_Or_List_Childs'First or else
           Go_To = N.And_Or_List_Childs (Index).Kind
         then
            Eval (S, T, N.And_Or_List_Childs (Index).N);
            if Get_Last_Exit_Status (S.all) = 0 then
               Go_To := AND_LIST;
            else
               Go_To := OR_LIST;
            end if;
         end if;
      end loop;

   end Eval_And_Or_List;

   -----------------
   -- Eval_Assign --
   -----------------

   procedure Eval_Assign
     (S : Shell_State_Access;
      N : Node;
      Do_Export : Boolean := False)
   is
      Tmp : Annotated_String_List;
   begin
      Set_Var_Value (S.all, "LINENO", To_String (Get_Lineno (N.Pos)));
      if N.Kind = CMD_NODE then
         Tmp := N.Cmd_Assign_List;
      else
         Tmp := N.Assign_List;
      end if;

      for A in 1 .. Length (Tmp) loop
         declare
            Assign : constant String := Str (Element (Tmp, A));
         begin
            for I in 1 .. Assign'Last loop
               if Assign (I) = '=' then
                  if I = Assign'Last then
                     Set_Var_Value (S.all, Assign (Assign'First .. I - 1),
                                    "",
                                    Do_Export);
                  else
                     Set_Var_Value
                       (S.all, Assign (Assign'First .. I - 1),
                        Eval_String_Unsplit
                          (S, Slice (Element (Tmp, A),
                           I + 1, Assign'Last)),
                        Do_Export);
                  end if;
                  exit;
               end if;
            end loop;
         end;
      end loop;
   end Eval_Assign;

   ----------------
   -- Eval_Brace --
   ----------------

   procedure Eval_Brace (S : Shell_State_Access; T : Shell_Tree; N : Node) is
      Current : constant Redirection_States := Get_Redirections (S.all);
   begin
      Set_Redirections (S, N.Redirections);
      Eval (S, T, N.Brace_Code);
      Restore_Redirections (S.all, Current);
   exception
      when E : Continue_Exception | Break_Exception =>
         Restore_Redirections (S.all, Current);
         Reraise_Occurrence (E);
      when Shell_Return_Exception =>
         Restore_Redirections (S.all, Current);
   end Eval_Brace;

   ---------------
   -- Eval_Case --
   ---------------

   procedure Eval_Case (S : Shell_State_Access; T : Shell_Tree; N : Node) is
      Case_Value : constant String := Eval_String_Unsplit (S, N.Case_Word);
      Current_Case : Node_Access := Get_Node (T, N.First_Case);
      Pattern_Found : Boolean := False;
      Current_Redirs : constant Redirection_States :=
        Get_Redirections (S.all);
   begin
      Set_Redirections (S, N.Redirections);
      while Current_Case /= null loop
         for I in 1 .. Length (Current_Case.Pattern_List) loop
            declare
               Str : constant String := Eval_String_Unsplit
                 (S, Element (Current_Case.Pattern_List, I), True);
               Reg : Posix_Shell.Regexp.Regexp;
            begin
               begin
                  Reg := Compile (Str, True);
               exception
                  when Error_In_Regexp =>
                     Ada.Text_IO.Put_Line (Str);
                     raise Program_Error;
               end;
               if Match (Case_Value, Reg) then
                  if Current_Case.Match_Code /= Null_Node then
                     Eval (S, T, Current_Case.Match_Code);
                  else
                     Save_Last_Exit_Status (S.all, 0);
                  end if;
                  Pattern_Found := True;
                  exit;
               end if;
            end;
         end loop;
         exit when Pattern_Found;
         Current_Case := Get_Node (T, Current_Case.Next_Patterns);
      end loop;

      --  In case no pattern has been matched and so no command executed we
      --  need to ensure that the exit status of the case construct is 0.
      if not Pattern_Found then
         Save_Last_Exit_Status (S.all, 0);
      end if;

      Restore_Redirections (S.all, Current_Redirs);
   exception
      when E :  Break_Exception | Continue_Exception =>
         Restore_Redirections (S.all, Current_Redirs);
         Reraise_Occurrence (E);
   end Eval_Case;

   --------------
   -- Eval_Cmd --
   --------------

   procedure Eval_Cmd (S            : Shell_State_Access;
                       T            : Shell_Tree;
                       Command      : String;
                       Arguments    : String_List;
                       Redirections : Redirection_Op_Stack)
   is
      Exit_Status : Integer := 0;
      Env : String_List := Get_Environment (S.all);
      Current_Redirs : constant Redirection_States := Get_Redirections (S.all);
   begin
      if Command = "exec" then
         Set_Redirections (S, Redirections, Free_Previous => True);
         Exit_Status := Run
           (S, Command, Arguments, Env);
      else
         Set_Redirections (S, Redirections);
         --  Ada.Text_IO.Put_Line (Get_Fd (S.all, 1)'Img);
         begin
            --  Export_Environment;
            Exit_Status := Run
              (S, Command, Arguments, Env);

         exception
            when others =>
               Restore_Redirections (S.all, Current_Redirs);
               raise;
         end;
         Restore_Redirections (S.all, Current_Redirs);

      end if;

      for J in Env'Range loop
         Free (Env (J));
      end loop;

      Save_Last_Exit_Status (S.all, Exit_Status);
   end Eval_Cmd;

   --------------
   -- Eval_Cmd --
   --------------

   procedure Eval_Cmd (S : Shell_State_Access; T : Shell_Tree; N : Node) is
   begin
      declare
         Result_String : String_List :=
           Eval_String_List (S, N.Cmd & N.Arguments);
         Args : constant String_List
           := Result_String (Result_String'First + 1 .. Result_String'Last);
         New_State : Shell_State_Access := S;
      begin
         Set_Var_Value (S.all, "LINENO", To_String (Get_Lineno (N.Pos)));
         if Length (N.Cmd_Assign_List) > 0 then
            New_State := new Shell_State;
            New_State.all := Enter_Scope (S.all);
            Eval_Assign (New_State, N, True);
         end if;

         if Is_Xtrace_Enabled (S.all) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "(pos " & Image (N.Pos) & ")");
         end if;

         if Result_String'Length > 0 then
            Eval_Cmd (S => New_State,
                      T => T,
                      Command => Result_String
                        (Result_String'First).all,
                      Arguments => Args,
                      Redirections => N.Redirections);
            for J in Result_String'Range loop
               Free (Result_String (J));
            end loop;
         end if;

         if Is_Xtrace_Enabled (S.all) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "(return " & Get_Last_Exit_Status (New_State.all)'Img & ")");
         end if;

         if Length (N.Cmd_Assign_List) > 0 then
            Leave_Scope (New_State.all, S.all, True);
         end if;
      end;

      exception
         when Variable_Name_Error =>
            --  The evaluation lead us to use an invalid variable name.
            --  In this case, do not attempt to run the command, and just
            --  return 1 as the exit code.  The error message has already
            --  been printed earlier (during the variable substitution),
            --  so no need to report anything further at this point.
            Save_Last_Exit_Status (S.all, 1);
   end Eval_Cmd;

   --------------
   -- Eval_For --
   --------------

   procedure Eval_For (S : Shell_State_Access; T : Shell_Tree; N : Node) is
      Loop_Var  : constant String := Str (N.Loop_Var);
      Loop_Var_Values : String_List := Eval_String_List (S, N.Loop_Var_Values);
      Is_Valid : Boolean;
      Break_Number : Integer;
      Current_Redirs : constant Redirection_States := Get_Redirections (S.all);

      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (S.all) + 1;
   begin
      Set_Redirections (S, N.Redirections);
      Set_Loop_Scope_Level (S.all, My_Nested_Level);
      for I in Loop_Var_Values'Range loop

         begin
            Set_Var_Value (S.all, Loop_Var, Loop_Var_Values (I).all);
            Eval (S, T, N.Loop_Code);
         exception
            when E : Continue_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  null;
               else
                  Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
                  Restore_Redirections (S.all, Current_Redirs);
                  raise Continue_Exception with To_String (Break_Number);
               end if;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  exit;
               else
                  Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
                  Restore_Redirections (S.all, Current_Redirs);
                  raise Break_Exception with To_String (Break_Number);
               end if;
         end;
      end loop;

      for Index in Loop_Var_Values'Range loop
         Free (Loop_Var_Values (Index));
      end loop;
      Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
      Restore_Redirections (S.all, Current_Redirs);
   end Eval_For;

   -------------------
   -- Eval_Function --
   -------------------

   procedure Eval_Function
     (S : Shell_State_Access; N : Node)
   is
   begin
      Register_Function (Str (N.Function_Name), N.Function_Code);
      Save_Last_Exit_Status (S.all, 0);
   end Eval_Function;

   -------------
   -- Eval_If --
   -------------

   procedure Eval_If (S : Shell_State_Access; T : Shell_Tree; N : Node) is
      Status      : Integer := 0;
      Current_Redirs : constant Redirection_States := Get_Redirections (S.all);
   begin
      Set_Redirections (S, N.Redirections);
      Eval (S, T, N.Cond);
      if Get_Last_Exit_Status (S.all) = 0 then
         Eval (S, T, N.True_Code);
         Status := Get_Last_Exit_Status (S.all);
      elsif N.False_Code /= Null_Node then
         Eval (S, T, N.False_Code);
         Status := Get_Last_Exit_Status (S.all);
      end if;

      Save_Last_Exit_Status (S.all, Status);
      Restore_Redirections (S.all, Current_Redirs);
   exception
      when E :  Break_Exception | Continue_Exception =>
         Restore_Redirections (S.all, Current_Redirs);
         Reraise_Occurrence (E);
   end Eval_If;

   ---------------
   -- Eval_List --
   ---------------

   procedure Eval_List (S : Shell_State_Access; T : Shell_Tree; N : Node) is
   begin
      for Index in N.List_Childs'Range loop
         Eval (S, T, N.List_Childs (Index));
      end loop;
   end Eval_List;

   ---------------
   -- Eval_Pipe --
   ---------------

   procedure Eval_Pipe (S : Shell_State_Access; T : Shell_Tree; N : Node) is
      Status : Integer;
      pragma Warnings (Off, Status);

      task type Eval_Task is
         entry Start (Pipe_Input : in out File_Descriptor; N : Node_Id);
      end Eval_Task;


      task body Eval_Task is
         S_Copy : constant Shell_State_Access := new Shell_State;
         Cmd_Node : Node_Id;
         Result : File_Descriptor;
         My_Input : File_Descriptor := -1;

      begin
         accept Start (Pipe_Input : in out File_Descriptor; N : Node_Id) do
            --  Create a new scope and create the pipe. The task is in charge
            --  of closing the writable side of the pipe and the main task the
            --  reading part
            GNAT.Task_Lock.Lock;
            S_Copy.all := Enter_Scope (S.all);
            Set_Pipe_Out (S_Copy.all);
            Result := Get_Fd (S_Copy.all, -2);
            if Pipe_Input /= -1 then
               My_Input := Pipe_Input;
               Set_Pipe_In (S_Copy.all, Pipe_Input);
            end if;

            --  Pass to the main task the fd to the readble side of the pipe.
            Pipe_Input := Result;
            Cmd_Node := N;
            GNAT.Task_Lock.Unlock;
         end Start;

         Eval (S_Copy, T, Cmd_Node);
         --  Ada.Text_IO.Put_Line ("task ends");
         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe).
         GNAT.Task_Lock.Lock;
         Close (S_Copy.all, 1);
         if My_Input /= -1 then
            Close (My_Input);
         end if;
         GNAT.Task_Lock.Unlock;


         --  accept Get_Exit_Status do
         --  Destroy scope and ensure exit_status is correctly set.
         --   Leave_Scope (S_Copy.all, S.all);
         --  end Get_Exit_Status
      end Eval_Task;

      Input_Fd : File_Descriptor := -1;

      My_Tasks : array (N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1) of
        Eval_Task;
   begin
      for J in N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1 loop
         My_Tasks (J).Start (Input_Fd, N.Pipe_Childs (J));
      end loop;


      declare
         S_Copy : constant Shell_State_Access := new Shell_State;
      begin
         GNAT.Task_Lock.Lock;
         S_Copy.all := Enter_Scope (S.all);
         Set_Pipe_In (S_Copy.all, Input_Fd);
         GNAT.Task_Lock.Unlock;
         Eval (S_Copy, T, N.Pipe_Childs (N.Pipe_Childs'Last));
         GNAT.Task_Lock.Lock;
         Close_Pipe (S_Copy.all);
         Leave_Scope (S_Copy.all, S.all);
         GNAT.Task_Lock.Unlock;
      end;

      if N.Pipe_Negation then
         if Get_Last_Exit_Status (S.all) /= 0 then
            Save_Last_Exit_Status (S.all, 0);
         else
            Save_Last_Exit_Status (S.all, 1);
         end if;

      end if;
   end Eval_Pipe;

   -------------------
   -- Eval_Subshell --
   -------------------

   procedure Eval_Subshell
     (S : Shell_State_Access; T : Shell_Tree; N : Node)
   is
      New_State : constant Shell_State_Access :=
        new Shell_State'(Enter_Scope (S.all));
      Current_Redirs : constant Redirection_States := Get_Redirections (S.all);
   begin
      Set_Redirections (New_State, N.Redirections);
      begin
         Eval (New_State, T, N.Subshell_Code);
      exception
         when Shell_Return_Exception =>
            --  A return outside of a function or a sourced script
            --  is not legitimate.
            Error (New_State.all, "return: can only `return'"
                   & " from a function or sourced script");
            Save_Last_Exit_Status (New_State.all, 1);
         when Shell_Exit_Exception =>
            null;
      end;
      Restore_Redirections (New_State.all, Current_Redirs);
      Leave_Scope (New_State.all, S.all);
   end Eval_Subshell;

   ----------------------
   -- Eval_Until_While --
   ----------------------

   procedure Eval_Until_While
     (S : Shell_State_Access; T : Shell_Tree; N : Node; Is_Until : Boolean)
   is
      Is_Valid : Boolean;
      Break_Number : Integer;
      Current_Redirs : constant Redirection_States := Get_Redirections (S.all);
      Result : Integer := 0;
      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (S.all) + 1;
   begin
      Set_Redirections (S, N.Redirections);
      Set_Loop_Scope_Level (S.all, My_Nested_Level);
      loop
         begin
            if not Is_Until then
               Eval (S, T, N.While_Cond);
               exit when Get_Last_Exit_Status (S.all) /= 0;
               Eval (S, T, N.While_Code);
               Result := Get_Last_Exit_Status (S.all);
            else
               Eval (S, T, N.Until_Code);
               Result := Get_Last_Exit_Status (S.all);
               Eval (S, T, N.Until_Cond);
               exit when Get_Last_Exit_Status (S.all) = 0;
            end if;
         exception
            when E : Continue_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  null;
               else
                  Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
                  Restore_Redirections (S.all, Current_Redirs);
                  raise Continue_Exception with To_String (Break_Number);
               end if;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  Result := Get_Last_Exit_Status (S.all);
                  exit;
               else
                  Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
                  Restore_Redirections (S.all, Current_Redirs);
                  raise Break_Exception with To_String (Break_Number);
               end if;
         end;
      end loop;
      Save_Last_Exit_Status (S.all, Result);
      Set_Loop_Scope_Level (S.all, My_Nested_Level - 1);
      Restore_Redirections (S.all, Current_Redirs);
   end Eval_Until_While;

end Posix_Shell.Tree.Evals;
