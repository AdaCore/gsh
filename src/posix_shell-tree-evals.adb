------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Tree.Evals                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Commands; use Posix_Shell.Commands;
with Posix_Shell.Subst; use Posix_Shell.Subst;
with Posix_Shell.Builtins; use Posix_Shell.Builtins;
with Posix_Shell.String_Utils; use Posix_Shell.String_Utils;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Task_Lock;
with GNAT.Regpat; use GNAT.Regpat;
with GNU; use GNU;
with OS.FS;
with OS;
with Posix_Shell.Exec; use Posix_Shell.Exec;
--  with Ada.Text_IO;

package body Posix_Shell.Tree.Evals is

   procedure Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      Id    : Node_Id);
   --  Eval a script represented by Tree in the State context starting from
   --  node Id
   --
   --  @param State the current state
   --  @param Tree the tree to be evaluated
   --  @param Id the node from which evaluation starts

   Special_Builtin_Matcher : constant Pattern_Matcher := Compile
     ("^(eval|exec|source|\.|:|break|continue|exit|export|readonly" &
        "|return|set|shift|times|trap|unset|cd)$");
   --  Regexp used to check if a command is a special builtin (see open group
   --  definition of special builtins). Note that we consider cd also as a
   --  special builtin. Indeed it comes handy to keep changes done on the shell
   --  state by cd. Note that in a very tricky case we won't be Posix as cd
   --  should not preserve variable changes ("a=b cd path" will preserver
   --  change on variable a).

   function Eval_Brace
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result;
   --  Evaluation of a BRACE_NODE node
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node to be evaluated
   --  @return the result of the evaluation

   function Eval_Case
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result;
   --  Evaluation of a CASE_NODE node
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node to be evaluated
   --  @return the result of the case evaluation

   function Eval_For
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
      return Eval_Result;
   --  Eval for construct
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node for which kind = FOR_NODE
   --  @return evaluation result

   function Eval_Function
     (State : in out Shell_State;
      N     : Node)
      return Eval_Result;
   --  Eval function declaration construct
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node for which kind = FOR_NODE
   --  @return evaluation result

   function Eval_If
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result;

   function Eval_Pipe
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result;
   --  Evaluation of a PIPE_NODE node
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node to be evaluated
   --  @return the result of the case evaluation

   function Eval_Subshell
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result;
   --  Eval subshell construct
   --
   --  @param State current shell state
   --  @param Tree current script AST
   --  @param N node for which kind = SUBSHELL_NODE
   --  @return evaluation result

   function Eval_Until_While
     (State       : in out Shell_State;
      Tree        : Shell_Tree;
      N           : Node;
      Is_Until    : Boolean)
      return Eval_Result;

   procedure Eval_Null_Cmd (S : in out Shell_State; N : Node);

   function Eval_Block
     (S : in out Shell_State; T : Shell_Tree; N : Node)
      return Eval_Result;

   procedure Eval_Assign
     (S         : in out Shell_State;
      T         : Shell_Tree;
      N         : Node;
      Do_Export : Boolean := False);
   --  @ Evaluate an ASSIGN_NODE

   function Eval_Cmd
     (State        : in out Shell_State;
      Command      : String;
      Arguments    : String_List;
      Redirections : Redirection_Stack)
      return Eval_Result;
   --  @ Execute a command.
   --  @
   --  @ :param State: current shell state
   --  @ :param Command: command to be executed.
   --  @ :param Arguments: list of arguments for the command
   --  @ :param Redirections: list of redirections to apply
   --  @ :raise: Shell_Exit, Break_Exception, Continue_Exception

   function Eval_Cmd
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
     return Eval_Result;
   --  @ Evaluate a CMD_NODE node
   --  @
   --  @ :param S: current shell state
   --  @ :param T: current script AST
   --  @ :param N: node to evaluate

   function Eval
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node_Id)
      return Eval_Result;

   function Eval
     (S : in out Shell_State; T : Shell_Tree; N : Node)
      return Eval_Result;

   ----------
   -- Eval --
   ----------

   procedure Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree)
   is
   begin
      Eval (State, Tree, Tree.Toplevel_Node);
   end Eval;

   function Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree)
      return Eval_Result
   is
   begin
      return Eval (State, Tree, Tree.Toplevel_Node);
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node_Id)
      return Eval_Result
   is
      Current_Node : Node_Id := N;
      Result       : Eval_Result;
   begin
      while Current_Node /= 0 loop
         Result := Eval (S, T, T.Node_Table.Table (Current_Node));
         if Result.Kind /= RESULT_STD then
            return Result;
         end if;

         declare
            Cont_If_True : constant Node_Id :=
              T.Node_Table.Table (Current_Node).Cont_If_True;
            Cont_If_False : constant Node_Id :=
              T.Node_Table.Table (Current_Node).Cont_If_False;

         begin
            if Cont_If_False = Cont_If_True then
               Current_Node := Cont_If_True;
            else
               if Get_Last_Exit_Status (S) = 0 then
                  Current_Node := Cont_If_True;
               else
                  Current_Node := Cont_If_False;
               end if;
            end if;
         end;
      end loop;
      return Result;
   end Eval;

   ----------
   -- Eval --
   ----------

   procedure Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      Id    : Node_Id)
   is
      Result : Eval_Result;
      pragma Unreferenced (Result);
   begin
      Result := Eval (State, Tree, Id);
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (State : in out Shell_State;
      Tree  : Shell_Tree)
      return String
   is

      Status : Integer;
      pragma Warnings (Off, Status);

      --  Read_Fd : OS.FS.File_Descriptor;

      task Eval_Task is
         entry Start (Pipe_Output : OS.FS.File_Descriptor);
         entry Get_Exit_Status (Exit_Status : out Integer);
      end Eval_Task;

      task body Eval_Task is
         S_Copy : Shell_State;
         Last_Status : Integer;
      begin
         accept Start (Pipe_Output : OS.FS.File_Descriptor) do
            --  Create a new scope and create the pipe. The task is in charge
            --  of closing the writable side of the pipe and the main task the
            --  reading part
            GNAT.Task_Lock.Lock;
            S_Copy := Enter_Scope (State);
            Set_Descriptor (S_Copy, 1, Pipe_Output);

            --  Pass to the main task the fd to the readble side of the pipe.
            --  Pipe_Input := Get_Fd (S_Copy, -2);
            GNAT.Task_Lock.Unlock;
         end Start;

         Eval (S_Copy, Tree);

         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe).
         --  OS.FS.Close (Get_Fd (S_Copy, 1));
         Last_Status := Leave_Scope (S_Copy);

         accept Get_Exit_Status (Exit_Status : out Integer) do
            --  Destroy scope and ensure exit_status is correctly set.
            --  GNAT.Task_Lock.Lock;
            Exit_Status := Last_Status;
            --  GNAT.Task_Lock.Unlock;
         end Get_Exit_Status;
      end Eval_Task;

      Pipe_Input, Pipe_Output : OS.FS.File_Descriptor;
   begin
      OS.FS.Open_Pipe (Pipe_Input, Pipe_Output);

      Eval_Task.Start (Pipe_Output);
      declare
         Output : constant String := Read_Pipe_And_Close (State, Pipe_Input);
         Exit_Status : Integer;
      begin
         Eval_Task.Get_Exit_Status (Exit_Status);
         Save_Last_Exit_Status (State, Exit_Status);
         return Strip_CR (Output);
      end;
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
      return Eval_Result
   is
      Result : Eval_Result := (RESULT_STD, 0);
   begin
      case N.Kind is
         when CASE_NODE        => Result := Eval_Case (S, T, N);
         when FOR_NODE         => Result := Eval_For (S, T, N);
         when IF_NODE          => Result := Eval_If (S, T, N);
         when PIPE_NODE        => Result := Eval_Pipe (S, T, N);
         when BRACE_NODE       => Result := Eval_Brace (S, T, N);
         when SUBSHELL_NODE    => Result := Eval_Subshell (S, T, N);
         when FUNCTION_NODE    => Result := Eval_Function (S, N);
         when ASSIGN_NODE      => Eval_Assign (S, T, N);
         when CMD_NODE         => Result := Eval_Cmd (S, T, N);
         when NULL_CMD_NODE    => Eval_Null_Cmd (S, N);
         when BLOCK_NODE       => Result := Eval_Block (S, T, N);
         when WHILE_NODE       => Result := Eval_Until_While (S, T, N, False);
         when UNTIL_NODE       => Result := Eval_Until_While (S, T, N, True);
         when others           => raise Program_Error;
      end case;
      return Result;
   end Eval;

   ----------------
   -- Eval_Block --
   ----------------

   function Eval_Block
     (S : in out Shell_State; T : Shell_Tree; N : Node) return Eval_Result
   is
      Descriptors     : Shell_Descriptors;
      Result          : Eval_Result;
   begin
      if N.Redirections /= Empty_Redirections then
         Descriptors := Get_Descriptors (S);
         if not Apply_Redirections (S, N.Redirections) then
            Save_Last_Exit_Status (S, 1);
            return (RESULT_STD, 1);
         end if;
      end if;

      Result := Eval (S, T, N.Code);

      if N.Redirections /= Empty_Redirections then
         Restore_Descriptors (S, Descriptors);
      end if;
      return Result;
   end Eval_Block;

   -----------------
   -- Eval_Assign --
   -----------------

   procedure Eval_Assign
     (S         : in out Shell_State;
      T         : Shell_Tree;
      N         : Node;
      Do_Export : Boolean := False)
   is
      Tmp    : Token_List;
      Pool   : constant List_Pool := Token_List_Pool (T);
      Force_Status : Boolean := True;
   begin
      Set_Var_Value (S, "LINENO", Line (N.Pos));
      if N.Kind = CMD_NODE then
         Tmp := N.Cmd_Assign_List;
      else
         Tmp := N.Assign_List;
      end if;

      while Tmp /= Null_List loop

         declare
            Assign : constant String :=
              Get_Token_String (Get_Element (Pool, Tmp));
            Has_Command_Subst : Boolean;
         begin
            for I in Assign'First .. Assign'Last loop
               if Assign (I) = '=' then
                  if I = Assign'Last then
                     Set_Var_Value (S, Assign (Assign'First .. I - 1),
                                    "",
                                    Do_Export);
                  else
                     Set_Var_Value
                       (S, Assign (Assign'First .. I - 1),
                        Eval_String_Unsplit
                          (S, Assign (I + 1 .. Assign'Last),
                           Has_Command_Subst => Has_Command_Subst),
                        Do_Export);
                     if Has_Command_Subst then
                        Force_Status := False;
                     end if;
                  end if;
                  exit;
               end if;
            end loop;
         end;
         Tmp := Next (Pool, Tmp);
      end loop;

      --  If during a substitution a command substitution is done then the exit
      --  status is the status of the last command run. Otherwise the
      --  status is set to 0.
      if Force_Status then
         Save_Last_Exit_Status (S, 0);
      end if;
   end Eval_Assign;

   ----------------
   -- Eval_Brace --
   ----------------

   function Eval_Brace
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
     return Eval_Result
   is
      Descriptors : constant Shell_Descriptors := Get_Descriptors (State);
      Result      : Eval_Result;
   begin

      if not Apply_Redirections (State, N.Redirections) then
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      Result := Eval (State, Tree, N.Brace_Code);
      Restore_Descriptors (State, Descriptors);
      return Result;
   end Eval_Brace;

   ---------------
   -- Eval_Case --
   ---------------

   function Eval_Case
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result
   is
      --  Set to True if the substitutions performed did command substitution
      Has_Command_Subst : Boolean;

      --  Evaluation of the expression on which the case is applied
      Case_Value        : constant String :=
        Eval_String_Unsplit (State, Get_Token_String (N.Case_Word),
                             Has_Command_Subst => Has_Command_Subst);

      --  Node corresponding to the current pattern to test
      Current_Case : Node := Get_Node (Tree, N.First_Case);

      --  Set to True whenever a pattern is matched
      Pattern_Found : Boolean := False;

      --  Save current descriptors to be restored at the end of the case
      Descriptors : constant Shell_Descriptors := Get_Descriptors (State);

      Result            : Eval_Result := (RESULT_STD, 0);
      Pool              : constant List_Pool := Token_List_Pool (Tree);
   begin
      --  Case statement can have redirections associated. Apply them if
      --  necessary.
      if not Apply_Redirections (State, N.Redirections) then
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      --  Loop of other the case patterns
      while Current_Case.Kind /= NOP_NODE loop
         declare
            Cursor : Token_List := Current_Case.Pattern_List;
         begin

            while Cursor /= Null_List loop

               declare
                  Str : constant String := Eval_String_Unsplit
                    (State,
                     Get_Token_String (Get_Element (Pool, Cursor)),
                     Case_Pattern      => True,
                     Has_Command_Subst => Has_Command_Subst);
               begin
                  if Fnmatch (Str, Case_Value) then
                     if Current_Case.Match_Code /= Null_Node then
                        Result := Eval (State, Tree, Current_Case.Match_Code);
                     else
                        Save_Last_Exit_Status (State, 0);
                        Result := (RESULT_STD, 0);
                     end if;
                     Pattern_Found := True;
                     exit;
                  end if;
               end;

               Cursor := Next (Pool, Cursor);
            end loop;
         end;
         exit when Pattern_Found;
         Current_Case := Get_Node (Tree, Current_Case.Next_Patterns);
      end loop;

      --  In case no pattern has been matched and so no command executed we
      --  need to ensure that the exit status of the case construct is 0.
      if not Pattern_Found then
         Save_Last_Exit_Status (State, 0);
         Result := (RESULT_STD, 0);
      end if;

      --  Finally restore the descriptors and return the result
      Restore_Descriptors (State, Descriptors);
      return Result;
   end Eval_Case;

   --------------
   -- Eval_Cmd --
   --------------

   function Eval_Cmd
     (State        : in out Shell_State;
      Command      : String;
      Arguments    : String_List;
      Redirections : Redirection_Stack)
     return Eval_Result
   is
      Exit_Status : Eval_Result;
      Env         : String_List := Get_Environment (State);
      Descriptors : constant Shell_Descriptors := Get_Descriptors (State);

      --  When the command to be executed is the exec special builtin, then
      --  the descriptors update with done in place (and thus descriptors not
      --  restored once the command is finished.
      Is_Exec     : constant Boolean := Command = "exec";

   begin
      if not Apply_Redirections (State, Redirections, In_Place => Is_Exec) then
         --  If descriptors update fails then do not execute the command and
         --  set the exit status to 1.
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      Exit_Status := Run (State, Command, Arguments, Env);
      Restore_Descriptors (State, Descriptors, In_Place => Is_Exec);

      --  Free the environment block
      for J in Env'Range loop
         Free (Env (J));
      end loop;

      --  And finally propagate the command exit status
      if Exit_Status.Kind = RESULT_STD then
         Save_Last_Exit_Status (State, Exit_Status.Status);
      end if;

      return Exit_Status;
   end Eval_Cmd;

   --------------
   -- Eval_Cmd --
   --------------

   function Eval_Cmd
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
     return Eval_Result
   is
      Pool   : constant List_Pool := Token_List_Pool (T);
      Result : Eval_Result;
   begin

      declare
         Result_String : String_List :=
           Eval_String (S, Get_Token_String (N.Cmd)) &
           Eval_String_List (S, T, N.Arguments);
      begin
         if Result_String'Length = 0 then
            return (RESULT_STD, 0);
         end if;

         declare
            Args        : constant String_List
              := Result_String (Result_String'First + 1 .. Result_String'Last);
            Cmd         : constant String :=
              Result_String (Result_String'First).all;
            New_State   : Shell_State;
            Has_Assigns : constant Boolean :=
              not Is_Empty (Pool, N.Cmd_Assign_List);
            Exit_Status : Integer;
         begin
            Set_Var_Value (S, "LINENO", Line (N.Pos));

            if Has_Assigns and then
              not Match (Special_Builtin_Matcher, Cmd)
            then
               --  When we execute a command that is not a special builtin and
               --  that has assignments associated we need to create a
               --  temporary scope. In that case no Shell_Exit,
               --  Break_Exception or Continue_Exeception can be raised.
               New_State := Enter_Scope (S);
               Eval_Assign (New_State, T, N, True);
               Result := Eval_Cmd (State        => New_State,
                                   Command      => Cmd,
                                   Arguments    => Args,
                                   Redirections => N.Redirections);

               for J in Result_String'Range loop
                  Free (Result_String (J));
               end loop;

               Exit_Status := Leave_Scope (New_State);
               Save_Last_Exit_Status (S, Exit_Status);
            else
               --  As stated by posix shell standard (2.14 Special Built-In
               --  Utilities). Variable assignments specified with special
               --  built-in utilities remain in effect after the built-in
               --  completes. Note that most of the shell such as bash do not
               --  respect this requirement.
               --  This part of the if statement also handle commands which do
               --  not have assignements associated. In that case there is
               --  indeed no need to create a new scope.

               if Has_Assigns then
                  Eval_Assign (S, T, N, True);
               end if;

               Result := Eval_Cmd (State        => S,
                                   Command      => Cmd,
                                   Arguments    => Args,
                                   Redirections => N.Redirections);

               for J in Result_String'Range loop
                  Free (Result_String (J));
               end loop;

            end if;
         end;
      end;

      --  Ada.Text_IO.Put_Line (Result.Kind'Img);
      if Result.Kind = RESULT_RETURN and not T.Allow_Return then
         Result := (RESULT_STD, 1);
         Error (S, "can only return from a function or a source script");
      end if;

      return Result;
   exception
      when Shell_Exit_Exception =>
         return (RESULT_EXIT, Get_Last_Exit_Status (S));
      when Variable_Name_Error =>
         --  The evaluation lead us to use an invalid variable name.
         --  In this case, do not attempt to run the command, and just
         --  return 1 as the exit code.  The error message has already
         --  been printed earlier (during the variable substitution),
         --  so no need to report anything further at this point.
         Save_Last_Exit_Status (S, 1);
         return (RESULT_STD, 1);
   end Eval_Cmd;

   --------------
   -- Eval_For --
   --------------

   function Eval_For (S : in out Shell_State; T : Shell_Tree; N : Node)
                      return Eval_Result
   is
      Loop_Var        : constant String := Get_Token_String (N.Loop_Var);
      Loop_Var_Values : String_List :=
        (if not N.Loop_Default_Values then
            Eval_String_List (S, T, N.Loop_Var_Values) else
              Eval_String (S, """$@"""));
      Descriptors     : constant Shell_Descriptors := Get_Descriptors (S);
      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (S) + 1;
      Result          : Eval_Result;
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return (RESULT_STD, 1);
      end if;
      Set_Loop_Scope_Level (S, My_Nested_Level);
      for I in Loop_Var_Values'Range loop
            Set_Var_Value (S, Loop_Var, Loop_Var_Values (I).all);
            Result := Eval (S, T, N.Loop_Code);
            if Result.Kind = RESULT_CONTINUE then
               if Result.Level = My_Nested_Level then
                  Result := (RESULT_STD, Get_Last_Exit_Status (S));
               else
                  exit;
               end if;
            end if;

            if Result.Kind = RESULT_BREAK then
               if Result.Level = My_Nested_Level then
                  Result := (RESULT_STD, Get_Last_Exit_Status (S));
               end if;
               exit;
            end if;
      end loop;

      for Index in Loop_Var_Values'Range loop
         Free (Loop_Var_Values (Index));
      end loop;
      Set_Loop_Scope_Level (S, My_Nested_Level - 1);
      Restore_Descriptors (S, Descriptors);
      return Result;
   end Eval_For;

   -------------------
   -- Eval_Function --
   -------------------

   function Eval_Function
     (State : in out Shell_State;
      N     : Node)
     return Eval_Result
   is
   begin
      Register_Function
        (State, Get_Token_String (N.Function_Name), N.Function_Code.all);
      Save_Last_Exit_Status (State, 0);
      return (RESULT_STD, 0);
   end Eval_Function;

   -------------
   -- Eval_If --
   -------------

   function Eval_If
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
     return Eval_Result
   is
      Result      : Eval_Result := (RESULT_STD, 0);
      Descriptors : constant Shell_Descriptors := Get_Descriptors (State);
   begin
      if not Apply_Redirections (State, N.Redirections) then
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      Eval (State, Tree, N.If_Condition);
      if Get_Last_Exit_Status (State) = 0 then
         Result := Eval (State, Tree, N.If_True_Code);
      elsif N.If_False_Code /= Null_Node then
         Result := Eval (State, Tree, N.If_False_Code);
      end if;

      if Result.Kind = RESULT_STD then
         Save_Last_Exit_Status (State, Result.Status);
      end if;
      Restore_Descriptors (State, Descriptors);
      return Result;
   end Eval_If;

   -------------------
   -- Eval_Null_Cmd --
   -------------------

   procedure Eval_Null_Cmd (S : in out Shell_State; N : Node)
   is
      Descriptors : constant Shell_Descriptors := Get_Descriptors (S);
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;
      Restore_Descriptors (S, Descriptors);
      Save_Last_Exit_Status (S, 0);
   end Eval_Null_Cmd;

   ---------------
   -- Eval_Pipe --
   ---------------

   function Eval_Pipe
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result
   is
      task type Eval_Task is
         entry Start
           (Pipe_Input  : OS.FS.File_Descriptor;
            Pipe_Output : OS.FS.File_Descriptor;
            N           : Node_Id);
         --  start a task part of pipeline
         --
         --  @param Pipe_Input
         --  @param Pipe_Output
      end Eval_Task;

      task body Eval_Task is
         Task_State : Shell_State;
         Task_Code  : Node_Id;

         Exit_Status : Integer;
         pragma Unreferenced (Exit_Status);

         use OS.FS;
      begin
         accept Start
           (Pipe_Input  : OS.FS.File_Descriptor;
            Pipe_Output : OS.FS.File_Descriptor;
            N           : Node_Id)
         do
            --  Create a new scope and override the stdout with the pipe output
            Task_State := Enter_Scope (State);
            Set_Descriptor (Task_State, 1, Pipe_Output);

            --  If this is not the first task in the pipe list then stdin
            --  should also be overriden
            if Pipe_Input /= OS.FS.Invalid_FD then
               Set_Descriptor (Task_State, 0, Pipe_Input);
            end if;

            Task_Code := N;
         end Start;

         begin
            Eval (Task_State, Tree, Task_Code);
         exception
            when others =>
               Error (Task_State, "crash in pipe command");
         end;

         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe
         Exit_Status := Leave_Scope (Task_State);

      end Eval_Task;

      Input_Fd      : OS.FS.File_Descriptor := OS.FS.Invalid_FD;
      Output_Fd     : OS.FS.File_Descriptor := OS.FS.Invalid_FD;
      Next_Input_Fd : OS.FS.File_Descriptor := OS.FS.Invalid_FD;

      My_Tasks : array (N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1) of
        Eval_Task;
   begin
      for J in N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1 loop
         OS.FS.Open_Pipe (Next_Input_Fd, Output_Fd);
         My_Tasks (J).Start (Input_Fd, Output_Fd, N.Pipe_Childs (J));
         Input_Fd := Next_Input_Fd;
      end loop;

      declare
         S_Copy : Shell_State;
         Exit_Status : Integer;
      begin
         S_Copy := Enter_Scope (State);
         Set_Descriptor (S_Copy, 0, Input_Fd);

         Eval (S_Copy, Tree, N.Pipe_Childs (N.Pipe_Childs'Last));
         Exit_Status := Leave_Scope (S_Copy);
         Save_Last_Exit_Status (State, Exit_Status);
      end;

      --  Handle pipe negation
      if N.Pipe_Negation then
         if Get_Last_Exit_Status (State) /= 0 then
            Save_Last_Exit_Status (State, 0);
         else
            Save_Last_Exit_Status (State, 1);
         end if;
      end if;
      return (RESULT_STD, Get_Last_Exit_Status (State));
   end Eval_Pipe;

   -------------------
   -- Eval_Subshell --
   -------------------

   function Eval_Subshell
     (State : in out Shell_State;
      Tree  : Shell_Tree;
      N     : Node)
      return Eval_Result
   is
      New_State   : Shell_State := Enter_Scope (State);
      Descriptors : constant Shell_Descriptors := Get_Descriptors (New_State);
      Exit_Status : Integer;
      Result      : Eval_Result;
   begin
      --  Apply redirections
      if not Apply_Redirections (New_State, N.Redirections) then
         Exit_Status := Leave_Scope (New_State);
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      --  Evaluate subshell code

      Result := Eval (New_State, Tree, N.Subshell_Code);

      case Result.Kind is
         when RESULT_STD | RESULT_EXIT =>
            Result := (RESULT_STD, Result.Status);
         when others =>
            Error (New_State, "invalid context for return, continue or break");
            Result := (RESULT_STD, 1);
      end case;

      --  Do we have a trap registered ?
      declare
         Exit_Trap_Action : constant String_Access :=
           Get_Trap_Action (New_State, 0);
         Trap_Status      : Eval_Result;
         pragma Warnings (Off, Trap_Status);
      begin
         if Exit_Trap_Action /= null and then Exit_Trap_Action.all'Length > 0
         then
            Trap_Status := Execute_Builtin
              (New_State,
               "eval",
               (1 => Exit_Trap_Action));

            case Trap_Status.Kind is
               when RESULT_STD | RESULT_EXIT =>
                  null;
               when RESULT_BREAK | RESULT_CONTINUE | RESULT_RETURN =>
                  Error (New_State,
                         "invalid context for return, continue or break");
                  Result := (RESULT_STD, 1);
            end case;
         end if;
      end;

      Save_Last_Exit_Status (New_State, Result.Status);
      Restore_Descriptors (New_State, Descriptors);
      Exit_Status := Leave_Scope (New_State);
      Save_Last_Exit_Status (State, Exit_Status);
      return (RESULT_STD, Exit_Status);
   end Eval_Subshell;

   ----------------------
   -- Eval_Until_While --
   ----------------------

   function Eval_Until_While
     (State    : in out Shell_State;
      Tree     : Shell_Tree;
      N        : Node;
      Is_Until : Boolean)
      return Eval_Result
   is
      --  Is_Valid        : Boolean;
      --  Break_Number    : Integer;
      Descriptors     : constant Shell_Descriptors := Get_Descriptors (State);
      --  Result          : Integer := 0;
      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (State) + 1;
      Result          : Eval_Result := (RESULT_STD, 0);
   begin
      if not Apply_Redirections (State, N.Redirections) then
         Save_Last_Exit_Status (State, 1);
         return (RESULT_STD, 1);
      end if;

      Set_Loop_Scope_Level (State, My_Nested_Level);

      loop
         begin
            if not Is_Until then
               Eval (State, Tree, N.While_Cond);
               exit when Get_Last_Exit_Status (State) /= 0;
               Result := Eval (State, Tree, N.While_Code);

            else
               Result := Eval (State, Tree, N.Until_Code);
               Eval (State, Tree, N.Until_Cond);
               exit when Get_Last_Exit_Status (State) = 0;
            end if;
         end;

         case Result.Kind is
            when RESULT_CONTINUE =>

               if Result.Level = My_Nested_Level then
                  Result := (RESULT_STD, Get_Last_Exit_Status (State));
               else
                  exit;
               end if;
            when RESULT_BREAK =>
               if Result.Level = My_Nested_Level then
                  Result := (RESULT_STD, Get_Last_Exit_Status (State));
               end if;
               exit;
            when RESULT_EXIT | RESULT_RETURN =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      if Result.Kind = RESULT_STD then
         Save_Last_Exit_Status (State, Result.Status);
      end if;

      Set_Loop_Scope_Level (State, My_Nested_Level - 1);
      Restore_Descriptors (State, Descriptors);
      return Result;
   end Eval_Until_While;
end Posix_Shell.Tree.Evals;
