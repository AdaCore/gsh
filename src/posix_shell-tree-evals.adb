------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Tree.Evals                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2015, AdaCore                   --
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
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Subst; use Posix_Shell.Subst;
with Posix_Shell.Utils; use  Posix_Shell.Utils;
with Posix_Shell.GNULib; use Posix_Shell.GNULib;
with Posix_Shell.Builtins; use Posix_Shell.Builtins;
with Posix_Shell.String_Utils; use Posix_Shell.String_Utils;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Task_Lock;
with GNAT.Regpat; use GNAT.Regpat;
with OS.FS;
with OS;

package body Posix_Shell.Tree.Evals is

   Special_Builtin_Matcher : constant Pattern_Matcher := Compile
     ("^(eval|exec|source|\.|:|break|continue|exit|export|readonly" &
        "|return|set|shift|times|trap|unset|cd)$");
   --  Regexp used to check if a command is a special builtin (see open group
   --  definition of special builtins). Note that we consider cd also as a
   --  special builtin. Indeed it comes handy to keep changes done on the shell
   --  state by cd. Note that in a very tricky case we won't be Posix as cd
   --  should not preserve variable changes ("a=b cd path" will preserver
   --  change on variable a).

   procedure Eval_List (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_Case (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_If (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_And_Or_List
     (S : in out Shell_State; T : Shell_Tree; N : Node);

   procedure Eval_Until_While
     (S : in out Shell_State; T : Shell_Tree; N : Node; Is_Until : Boolean);
   procedure Eval_Pipe (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_For (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_Subshell (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_Function (S : in out Shell_State; N : Node);
   procedure Eval_Brace (S : in out Shell_State; T : Shell_Tree; N : Node);
   procedure Eval_Null_Cmd (S : in out Shell_State; N : Node);

   procedure Eval_Assign
     (S         : in out Shell_State;
      T         : Shell_Tree;
      N         : Node;
      Do_Export : Boolean := False);
   --  @ Evaluate an ASSIGN_NODE

   procedure Eval_Cmd
     (State        : in out Shell_State;
      Command      : String;
      Arguments    : String_List;
      Redirections : Redirection_Stack);
   --  @ Execute a command.
   --  @
   --  @ :param State: current shell state
   --  @ :param Command: command to be executed.
   --  @ :param Arguments: list of arguments for the command
   --  @ :param Redirections: list of redirections to apply
   --  @ :raise: Shell_Exit, Break_Exception, Continue_Exception

   procedure Eval_Cmd
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node);
   --  @ Evaluate a CMD_NODE node
   --  @
   --  @ :param S: current shell state
   --  @ :param T: current script AST
   --  @ :param N: node to evaluate

   ----------
   -- Eval --
   ----------

   procedure Eval (S : in out Shell_State; T : Shell_Tree) is
   begin
      Eval (S, T, T.Toplevel_Node);
   end Eval;

   ----------
   -- Eval --
   ----------

   procedure Eval
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node_Id)
   is
   begin
      if N = 0 then
         return;
      else
         Eval (S, T, T.Node_Table.Table (N));
      end if;
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (S : in out Shell_State;
      T : Shell_Tree) return String
   is

      Status : Integer;
      pragma Warnings (Off, Status);

      Read_Fd : OS.FS.File_Descriptor;

      task Eval_Task is
         entry Start (Pipe_Input : out OS.FS.File_Descriptor);
         entry Get_Exit_Status;
      end Eval_Task;

      task body Eval_Task is
         S_Copy : Shell_State;
      begin
         accept Start (Pipe_Input : out OS.FS.File_Descriptor) do
            --  Create a new scope and create the pipe. The task is in charge
            --  of closing the writable side of the pipe and the main task the
            --  reading part
            GNAT.Task_Lock.Lock;
            S_Copy := Enter_Scope (S);
            Set_Pipe_Out (S_Copy);

            --  Pass to the main task the fd to the readble side of the pipe.
            Pipe_Input := Get_Fd (S_Copy, -2);
            GNAT.Task_Lock.Unlock;
         end Start;

         Eval (S_Copy, T);

         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe).
         OS.FS.Close (Get_Fd (S_Copy, 1));

         accept Get_Exit_Status do
            --  Destroy scope and ensure exit_status is correctly set.
            GNAT.Task_Lock.Lock;
            Leave_Scope (S_Copy, S);
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

   procedure Eval (S : in out Shell_State; T : Shell_Tree; N : Node) is
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
         when ASSIGN_NODE      => Eval_Assign (S, T, N);
         when CMD_NODE         => Eval_Cmd (S, T, N);
         when NULL_CMD_NODE    => Eval_Null_Cmd (S, N);
         when others           => raise Program_Error;
      end case;
      return;
   end Eval;

   -------------------
   -- Eval_And_List --
   -------------------

   procedure Eval_And_Or_List
     (S : in out Shell_State; T : Shell_Tree; N : Node)
   is
      Go_To  : List_Kind := AND_LIST;
   begin
      for Index in N.And_Or_List_Childs'Range loop
         if Index = N.And_Or_List_Childs'First or else
           Go_To = N.And_Or_List_Childs (Index).Kind
         then
            Eval (S, T, N.And_Or_List_Childs (Index).N);
            if Get_Last_Exit_Status (S) = 0 then
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

   procedure Eval_Brace (S : in out Shell_State; T : Shell_Tree; N : Node) is
      Descriptors : constant Shell_Descriptors := Get_Descriptors (S);

   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;

      Eval (S, T, N.Brace_Code);
      Restore_Descriptors (S, Descriptors);
   exception
      when E : Continue_Exception | Break_Exception =>
         Restore_Descriptors (S, Descriptors);
         Reraise_Occurrence (E);
      when Shell_Return_Exception =>
         Restore_Descriptors (S, Descriptors);
   end Eval_Brace;

   ---------------
   -- Eval_Case --
   ---------------

   procedure Eval_Case
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
   is
      Has_Command_Subst : Boolean;
      Case_Value        : constant String :=
        Eval_String_Unsplit (S, Get_Token_String (N.Case_Word),
                             Has_Command_Subst => Has_Command_Subst);
      Current_Case      : Node := Get_Node (T, N.First_Case);
      Pattern_Found     : Boolean := False;
      Descriptors       : constant Shell_Descriptors := Get_Descriptors (S);
      Cursor            : Token_List;
      Pool              : constant List_Pool := Token_List_Pool (T);
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;

      while Current_Case.Kind /= NOP_NODE loop
         Cursor := Current_Case.Pattern_List;

         while Cursor /= Null_List loop

            declare
               Str : constant String := Eval_String_Unsplit
                 (S, Get_Token_String (Get_Element (Pool, Cursor)),
                  True,
                  Has_Command_Subst => Has_Command_Subst);
            begin
               if Fnmatch (Str, Case_Value) then
                  if Current_Case.Match_Code /= Null_Node then
                     Eval (S, T, Current_Case.Match_Code);
                  else
                     Save_Last_Exit_Status (S, 0);
                  end if;
                  Pattern_Found := True;
                  exit;
               end if;
            end;

            Cursor := Next (Pool, Cursor);
         end loop;

         exit when Pattern_Found;
         Current_Case := Get_Node (T, Current_Case.Next_Patterns);
      end loop;

      --  In case no pattern has been matched and so no command executed we
      --  need to ensure that the exit status of the case construct is 0.
      if not Pattern_Found then
         Save_Last_Exit_Status (S, 0);
      end if;

      Restore_Descriptors (S, Descriptors);
   exception
      when E :  Break_Exception | Continue_Exception =>
         Restore_Descriptors (S, Descriptors);
         Reraise_Occurrence (E);
   end Eval_Case;

   --------------
   -- Eval_Cmd --
   --------------

   procedure Eval_Cmd
     (State        : in out Shell_State;
      Command      : String;
      Arguments    : String_List;
      Redirections : Redirection_Stack)
   is
      Exit_Status : Integer;
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
         return;
      end if;

      begin
         Exit_Status := Run (State, Command, Arguments, Env);
      exception
         when others =>
            Restore_Descriptors (State, Descriptors, In_Place => Is_Exec);
            raise;
      end;
      Restore_Descriptors (State, Descriptors, In_Place => Is_Exec);

      --  Free the environment block
      for J in Env'Range loop
         Free (Env (J));
      end loop;

      --  And finally propagate the command exit status
      Save_Last_Exit_Status (State, Exit_Status);
   end Eval_Cmd;

   --------------
   -- Eval_Cmd --
   --------------

   procedure Eval_Cmd
     (S : in out Shell_State;
      T : Shell_Tree;
      N : Node)
   is
      Pool : constant List_Pool := Token_List_Pool (T);

   begin

      declare
         Result_String : String_List :=
           Eval_String (S, Get_Token_String (N.Cmd)) &
           Eval_String_List (S, T, N.Arguments);
      begin
         if Result_String'Length = 0 then
            return;
         end if;

         declare
            Args        : constant String_List
              := Result_String (Result_String'First + 1 .. Result_String'Last);
            Cmd         : constant String :=
              Result_String (Result_String'First).all;
            New_State   : Shell_State;
            Has_Assigns : constant Boolean :=
              not Is_Empty (Pool, N.Cmd_Assign_List);
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
               Eval_Cmd (State        => New_State,
                         Command      => Cmd,
                         Arguments    => Args,
                         Redirections => N.Redirections);

               for J in Result_String'Range loop
                  Free (Result_String (J));
               end loop;

               Leave_Scope (New_State, S);
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

               begin
                  Eval_Cmd (State        => S,
                            Command      => Cmd,
                            Arguments    => Args,
                            Redirections => N.Redirections);
               exception
                  when others =>
                     --  We can catch here exceptions not associated to errors
                     --  such as Shell_Exit, Continue_Exception or
                     --  Break_Exception
                     for J in Result_String'Range loop
                        Free (Result_String (J));
                     end loop;
                     raise;
               end;

               for J in Result_String'Range loop
                  Free (Result_String (J));
               end loop;

            end if;
         end;
      end;

   exception
      when Variable_Name_Error =>
         --  The evaluation lead us to use an invalid variable name.
         --  In this case, do not attempt to run the command, and just
         --  return 1 as the exit code.  The error message has already
         --  been printed earlier (during the variable substitution),
         --  so no need to report anything further at this point.
         Save_Last_Exit_Status (S, 1);
   end Eval_Cmd;

   --------------
   -- Eval_For --
   --------------

   procedure Eval_For (S : in out Shell_State; T : Shell_Tree; N : Node) is
      Loop_Var        : constant String := Get_Token_String (N.Loop_Var);
      Loop_Var_Values : String_List :=
        (if not N.Loop_Default_Values then
            Eval_String_List (S, T, N.Loop_Var_Values) else
              Eval_String (S, """$@"""));
      Is_Valid        : Boolean;
      Break_Number    : Integer;
      Descriptors     : constant Shell_Descriptors := Get_Descriptors (S);
      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (S) + 1;
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;
      Set_Loop_Scope_Level (S, My_Nested_Level);
      for I in Loop_Var_Values'Range loop

         begin
            Set_Var_Value (S, Loop_Var, Loop_Var_Values (I).all);
            Eval (S, T, N.Loop_Code);
         exception
            when E : Continue_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  null;
               else
                  Set_Loop_Scope_Level (S, My_Nested_Level - 1);
                  Restore_Descriptors (S, Descriptors);
                  raise Continue_Exception with To_String (Break_Number);
               end if;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  exit;
               else
                  Set_Loop_Scope_Level (S, My_Nested_Level - 1);
                  Restore_Descriptors (S, Descriptors);
                  raise Break_Exception with To_String (Break_Number);
               end if;
         end;
      end loop;

      for Index in Loop_Var_Values'Range loop
         Free (Loop_Var_Values (Index));
      end loop;
      Set_Loop_Scope_Level (S, My_Nested_Level - 1);
      Restore_Descriptors (S, Descriptors);
   end Eval_For;

   -------------------
   -- Eval_Function --
   -------------------

   procedure Eval_Function
     (S : in out Shell_State; N : Node)
   is
   begin
      Register_Function (S,
                         Get_Token_String (N.Function_Name),
                         N.Function_Code.all);
      Save_Last_Exit_Status (S, 0);
   end Eval_Function;

   -------------
   -- Eval_If --
   -------------

   procedure Eval_If (S : in out Shell_State; T : Shell_Tree; N : Node) is
      Status      : Integer := 0;
      Descriptors : constant Shell_Descriptors := Get_Descriptors (S);
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;
      Eval (S, T, N.Cond);
      if Get_Last_Exit_Status (S) = 0 then
         Eval (S, T, N.True_Code);
         Status := Get_Last_Exit_Status (S);
      elsif N.False_Code /= Null_Node then
         Eval (S, T, N.False_Code);
         Status := Get_Last_Exit_Status (S);
      end if;

      Save_Last_Exit_Status (S, Status);
      Restore_Descriptors (S, Descriptors);
   exception
      when E :  Break_Exception | Continue_Exception =>
         Restore_Descriptors (S, Descriptors);
         Reraise_Occurrence (E);
   end Eval_If;

   ---------------
   -- Eval_List --
   ---------------

   procedure Eval_List (S : in out Shell_State; T : Shell_Tree; N : Node) is
   begin
      for Index in N.List_Childs'Range loop
         Eval (S, T, N.List_Childs (Index));
      end loop;
   end Eval_List;

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

   procedure Eval_Pipe (S : in out Shell_State; T : Shell_Tree; N : Node) is
      Status : Integer;
      pragma Warnings (Off, Status);

      task type Eval_Task is
         entry Start (Pipe_Input : in out OS.FS.File_Descriptor; N : Node_Id);
      end Eval_Task;

      task body Eval_Task is
         S_Copy   : Shell_State;
         Cmd_Node : Node_Id;
         Result   : OS.FS.File_Descriptor;
         My_Input : OS.FS.File_Descriptor := OS.FS.Invalid_FD;

         use OS.FS;
      begin
         accept Start (Pipe_Input : in out OS.FS.File_Descriptor; N : Node_Id)
         do
            --  Create a new scope and create the pipe. The task is in charge
            --  of closing the writable side of the pipe and the main task the
            --  reading part
            GNAT.Task_Lock.Lock;
            S_Copy := Enter_Scope (S);
            Set_Pipe_Out (S_Copy);
            Result := Get_Fd (S_Copy, -2);
            if Pipe_Input /= OS.FS.Invalid_FD then
               My_Input := Pipe_Input;
               Set_Pipe_In (S_Copy, Pipe_Input);
            end if;

            --  Pass to the main task the fd to the readble side of the pipe.
            Pipe_Input := Result;
            Cmd_Node := N;
            GNAT.Task_Lock.Unlock;
         end Start;

         begin
            Eval (S_Copy, T, Cmd_Node);
         exception
            when others =>
               Ada.Text_IO.Put_Line ("got exception");
         end;
         --  Ada.Text_IO.Put_Line ("task ends");
         --  Close the write side (will unblock main task which is doing a read
         --  of the other side of the pipe).
         GNAT.Task_Lock.Lock;
         Close (S_Copy, 1);
         if My_Input /= -1 then
            Close (My_Input);
         end if;
         GNAT.Task_Lock.Unlock;

         --  accept Get_Exit_Status do
         --  Destroy scope and ensure exit_status is correctly set.
         --   Leave_Scope (S_Copy, S);
         --  end Get_Exit_Status
      end Eval_Task;

      Input_Fd : OS.FS.File_Descriptor := OS.FS.Invalid_FD;

      My_Tasks : array (N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1) of
        Eval_Task;
   begin
      for J in N.Pipe_Childs'First .. N.Pipe_Childs'Last - 1 loop
         My_Tasks (J).Start (Input_Fd, N.Pipe_Childs (J));
      end loop;

      declare
         S_Copy : Shell_State;
      begin
         GNAT.Task_Lock.Lock;
         S_Copy := Enter_Scope (S);
         Set_Pipe_In (S_Copy, Input_Fd);
         GNAT.Task_Lock.Unlock;
         Eval (S_Copy, T, N.Pipe_Childs (N.Pipe_Childs'Last));
         GNAT.Task_Lock.Lock;
         Close_Pipe (S_Copy);
         Leave_Scope (S_Copy, S);
         GNAT.Task_Lock.Unlock;
      end;

      if N.Pipe_Negation then
         if Get_Last_Exit_Status (S) /= 0 then
            Save_Last_Exit_Status (S, 0);
         else
            Save_Last_Exit_Status (S, 1);
         end if;

      end if;
   end Eval_Pipe;

   -------------------
   -- Eval_Subshell --
   -------------------

   procedure Eval_Subshell
     (S : in out Shell_State; T : Shell_Tree; N : Node)
   is
      New_State : Shell_State := Enter_Scope (S);
      Descriptors : constant Shell_Descriptors := Get_Descriptors (S);
   begin
      if not Apply_Redirections (New_State, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;
      begin
         Eval (New_State, T, N.Subshell_Code);
      exception
         when Shell_Return_Exception =>
            --  A return outside of a function or a sourced script
            --  is not legitimate.
            Error (New_State, "return: can only `return'"
                   & " from a function or sourced script");
            Save_Last_Exit_Status (New_State, 1);
         when Shell_Exit_Exception =>
            null;
      end;

      --  Do we have a trap registered ?
      declare
         Exit_Trap_Action : constant String_Access :=
           Get_Trap_Action (New_State, 0);
         Trap_Status : Integer;
         Saved_Status : Integer;
         pragma Warnings (Off, Trap_Status);
      begin
         if Exit_Trap_Action /= null and then
           Exit_Trap_Action.all'Length > 0
         then
            Saved_Status := Get_Last_Exit_Status (New_State);
            Trap_Status := Execute_Builtin (New_State,
                                           "eval",
                                            (1 => Exit_Trap_Action));
            Save_Last_Exit_Status (New_State, Saved_Status);
         end if;
      exception
         when Shell_Return_Exception =>
            --  A return outside of a function or a sourced script
            --  is not legitimate.
            Error (New_State, "return: can only `return'"
                   & " from a function or sourced script");
            Save_Last_Exit_Status (New_State, 1);
         when Shell_Exit_Exception =>
            null;
      end;

      Restore_Descriptors (New_State, Descriptors);
      Leave_Scope (New_State, S);
   end Eval_Subshell;

   ----------------------
   -- Eval_Until_While --
   ----------------------

   procedure Eval_Until_While
     (S : in out Shell_State; T : Shell_Tree; N : Node; Is_Until : Boolean)
   is
      Is_Valid : Boolean;
      Break_Number : Integer;
      Descriptors : constant Shell_Descriptors := Get_Descriptors (S);
      Result : Integer := 0;
      My_Nested_Level : constant Natural := Get_Loop_Scope_Level (S) + 1;
   begin
      if not Apply_Redirections (S, N.Redirections) then
         Save_Last_Exit_Status (S, 1);
         return;
      end if;
      Set_Loop_Scope_Level (S, My_Nested_Level);
      loop
         begin
            if not Is_Until then
               Eval (S, T, N.While_Cond);
               exit when Get_Last_Exit_Status (S) /= 0;
               Eval (S, T, N.While_Code);
               Result := Get_Last_Exit_Status (S);
            else
               Eval (S, T, N.Until_Code);
               Result := Get_Last_Exit_Status (S);
               Eval (S, T, N.Until_Cond);
               exit when Get_Last_Exit_Status (S) = 0;
            end if;
         exception
            when E : Continue_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  null;
               else
                  Set_Loop_Scope_Level (S, My_Nested_Level - 1);
                  Restore_Descriptors (S, Descriptors);
                  raise Continue_Exception with To_String (Break_Number);
               end if;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = My_Nested_Level then
                  Result := Get_Last_Exit_Status (S);
                  exit;
               else
                  Set_Loop_Scope_Level (S, My_Nested_Level - 1);
                  Restore_Descriptors (S, Descriptors);
                  raise Break_Exception with To_String (Break_Number);
               end if;
         end;
      end loop;
      Save_Last_Exit_Status (S, Result);
      Set_Loop_Scope_Level (S, My_Nested_Level - 1);
      Restore_Descriptors (S, Descriptors);
   end Eval_Until_While;

end Posix_Shell.Tree.Evals;
