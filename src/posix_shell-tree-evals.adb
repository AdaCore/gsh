with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Regexp; use Posix_Shell.Regexp;

with Posix_Shell.Commands_Preprocessor; use Posix_Shell.Commands_Preprocessor;
with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Functions; use Posix_Shell.Functions;
with Posix_Shell.Subst; use Posix_Shell.Subst;
with Posix_Shell.Variables; use Posix_Shell.Variables;
with Posix_Shell.Utils; use  Posix_Shell.Utils;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Posix_Shell.Tree.Evals is

   function Eval_List (N : Node) return Integer;
   function Eval_Case (N : Node) return Integer;
   function Eval_If (N : Node) return Integer;
   function Eval_And_Or_List (N : Node) return Integer;
   function Eval_Until_While (N : Node; Is_Until : Boolean) return Integer;
   function Eval_Pipe (N : Node) return Integer;
   function Eval_For (N : Node) return Integer;
   function Eval_Subshell (N : Node) return Integer;
   function Eval_Function (N : Node) return Integer;
   function Eval_Brace (N : Node) return Integer;

   function Eval_Cmd (Command      : String;
                      Arguments    : String_List;
                      Redirections : Redirection_Op_Stack)
                      return Integer;
   --  Evaluate the call to the given Command with the given Arguments.
   --  This function takes care of setting and the restoring the redirections
   --  as well.

   function Eval_Cmd (N : Node) return Integer;
   --  Evaluate the given node as a command, after having expanded
   --  the command name and its arguments.

   function Eval_Assign
     (N : Node;
      Do_Export : Boolean := False)
      return Integer;

   ----------
   -- Eval --
   ----------

   function Eval (N : Node_Id) return Integer is
   begin
      return Eval (Node_Table.Table (N).all);
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval (N : Node_Id) return String is
      Status : Integer;
      pragma Warnings (Off, Status);
      Current_Dir : constant String := Get_Current_Dir;
   begin
      Enter_Scope;
      Set_Pipe_Out;
      Status := Eval (N);
      declare
         Result : constant String := Read_Pipe_And_Close;
      begin
         Change_Dir (Current_Dir);
         Leave_Scope;
         return Strip_CR (Result);
      end;
   end Eval;

   ----------
   -- Eval --
   ----------

   function Eval (N : Node) return Integer is
   begin
      case N.Kind is
         when LIST_NODE => return Eval_List (N);
         when CASE_NODE => return Eval_Case (N);
         when IF_NODE   => return Eval_If (N);
         when AND_OR_LIST_NODE => return Eval_And_Or_List (N);
         when WHILE_NODE => return Eval_Until_While (N, False);
         when UNTIL_NODE => return Eval_Until_While (N, True);
         when FOR_NODE => return Eval_For (N);
         when PIPE_NODE  => return Eval_Pipe (N);
         when BRACE_NODE => return Eval_Brace (N);
         when SUBSHELL_NODE => return Eval_Subshell (N);
         when FUNCTION_NODE => return Eval_Function (N);
         when ASSIGN_NODE => return Eval_Assign (N);
         when CMD_NODE => return Eval_Cmd (N);
         when NULL_CMD_NODE => return 0;
         when others    => raise Program_Error;
      end case;
   end Eval;

   -------------------
   -- Eval_And_List --
   -------------------

   function Eval_And_Or_List (N : Node) return Integer is
      Status : Integer := 0;
      Go_To : List_Kind := AND_LIST;
   begin
      for Index in N.And_Or_List_Childs'Range loop
         if Index = N.And_Or_List_Childs'First or else
           Go_To = N.And_Or_List_Childs (Index).Kind
         then
            Status := Eval (N.And_Or_List_Childs (Index).N);
            if Status = 0 then
               Go_To := AND_LIST;
            else
               Go_To := OR_LIST;
            end if;
         end if;
      end loop;

      return Status;
   end Eval_And_Or_List;

   -----------------
   -- Eval_Assign --
   -----------------

   function Eval_Assign
     (N : Node;
      Do_Export : Boolean := False)
      return Integer
   is
      Tmp : Annotated_String_List;
   begin
      Set_Var_Value ("LINENO", To_String (Get_Lineno (N.Pos)));
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
                     Set_Var_Value (Assign (Assign'First .. I - 1),
                                    "",
                                    Do_Export);
                  else
                     Set_Var_Value
                       (Assign (Assign'First .. I - 1),
                        Eval_String_Unsplit
                          (Slice (Element (Tmp, A),
                           I + 1, Assign'Last)),
                        Do_Export);
                  end if;
                  exit;
               end if;
            end loop;
         end;
      end loop;
      return 0;
   end Eval_Assign;

   ----------------
   -- Eval_Brace --
   ----------------

   function Eval_Brace (N : Node) return Integer is
      Result : Integer;
   begin
      Push_Redirections (N.Redirections);
      Result := Eval (N.Brace_Code);
      Pop_Redirections;
      return Result;
   exception
      when Shell_Return_Exception =>
         Pop_Redirections;
         return Get_Last_Exit_Status;
   end Eval_Brace;

   ---------------
   -- Eval_Case --
   ---------------

   function Eval_Case (N : Node) return Integer is
      Case_Value : constant String := Eval_String_Unsplit (N.Case_Word);
      Current_Case : Node_Access := Get_Node (N.First_Case);
      Exit_Status : Integer := 0;
      Pattern_Found : Boolean := False;
   begin
      Push_Redirections (N.Redirections);
      while Current_Case /= null loop
         for I in 1 .. Length (Current_Case.Pattern_List) loop
            declare
               Str : constant String := Eval_String_Unsplit
                 (Element (Current_Case.Pattern_List, I), True);
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
                     Exit_Status := Eval (Current_Case.Match_Code);
                  else
                     Exit_Status := 0;
                  end if;
                  Pattern_Found := True;
                  exit;
               end if;
            end;
         end loop;
         exit when Pattern_Found;
         Current_Case := Get_Node (Current_Case.Next_Patterns);
      end loop;
      Pop_Redirections;
      return Exit_Status;
   exception
      when E :  Break_Exception | Continue_Exception =>
         Pop_Redirections;
         Reraise_Occurrence (E);
   end Eval_Case;

   --------------
   -- Eval_Cmd --
   --------------

   function Eval_Cmd (Command      : String;
                      Arguments    : String_List;
                      Redirections : Redirection_Op_Stack)
                      return Integer is
      Exit_Status : Integer := 0;
   begin
      if Command = "exec" then
         Set_Redirections (Redirections);
         Exit_Status := Run (Command, Arguments, Get_Environment);
         Save_Last_Exit_Status (Exit_Status);
         return Exit_Status;
      else
         Push_Redirections (Redirections);
         begin
            --  Export_Environment;
            Exit_Status := Run (Command, Arguments, Get_Environment);
         exception
            when others =>
               Pop_Redirections;
               raise;
         end;
         Pop_Redirections;
         Save_Last_Exit_Status (Exit_Status);
         return Exit_Status;
      end if;
   end Eval_Cmd;

   --------------
   -- Eval_Cmd --
   --------------

   function Eval_Cmd (N : Node) return Integer is
   begin
      declare
         Result_String : String_List :=
           Eval_String_List (N.Cmd & N.Arguments);
         Args : constant String_List
           := Result_String (Result_String'First + 1 .. Result_String'Last);
         Temp : Integer;
      begin
         Set_Var_Value ("LINENO", To_String (Get_Lineno (N.Pos)));
         if Length (N.Cmd_Assign_List) > 0 then
            Enter_Scope;
            Temp := Eval_Assign (N, True);
         end if;

         if Result_String'Length > 0 then
            Temp :=  Eval_Cmd (Command => Result_String
                               (Result_String'First).all,
                               Arguments => Args,
                               Redirections => N.Redirections);
            for J in Result_String'Range loop
               Free (Result_String (J));
            end loop;
         else
            Temp := 0;
         end if;

         if Length (N.Cmd_Assign_List) > 0 then
            Leave_Scope (True);
         end if;
         return Temp;
      end;

      exception
         when Variable_Name_Error =>
            --  The evaluation lead us to use an invalid variable name.
            --  In this case, do not attempt to run the command, and just
            --  return 1 as the exit code.  The error message has already
            --  been printed earlier (during the variable substitution),
            --  so no need to report anything further at this point.
            Save_Last_Exit_Status (1);
            return 1;
   end Eval_Cmd;

   --------------
   -- Eval_For --
   --------------

   function Eval_For (N : Node) return Integer is
      Status : Integer := 0;
      Loop_Var  : constant String := Str (N.Loop_Var);
      Loop_Var_Values : constant String_List :=
        Eval_String_List (N.Loop_Var_Values);
      Is_Valid : Boolean;
      Break_Number : Integer;
   begin
      Push_Redirections (N.Redirections);
      for I in Loop_Var_Values'Range loop
         begin
            Set_Var_Value (Loop_Var, Loop_Var_Values (I).all);
            Status := Eval (N.Loop_Code);
         exception
            when Continue_Exception => null;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = 1 then
                  exit;
               else
                  Pop_Redirections;
                  raise Break_Exception with To_String (Break_Number - 1);
               end if;
         end;
      end loop;
      Pop_Redirections;
      return Status;
   end Eval_For;

   -------------------
   -- Eval_Function --
   -------------------

   function Eval_Function (N : Node) return Integer is
   begin
      Register_Function (Str (N.Function_Name), Get_Node (N.Function_Code));
      return 0;
   end Eval_Function;

   -------------
   -- Eval_If --
   -------------

   function Eval_If (N : Node) return Integer is
      Status      : Integer := 0;
      Cond_Status : Integer;
   begin
      Push_Redirections (N.Redirections);
      Cond_Status := Eval (N.Cond);
      if Cond_Status = 0 then
         Status := Eval (N.True_Code);
      elsif N.False_Code /= Null_Node then
         Status := Eval (N.False_Code);
      end if;

      Save_Last_Exit_Status (Status);
      Pop_Redirections;

      return Status;
   exception
      when E :  Break_Exception | Continue_Exception =>
         Pop_Redirections;
         Reraise_Occurrence (E);
   end Eval_If;

   ---------------
   -- Eval_List --
   ---------------

   function Eval_List (N : Node) return Integer is
      Status : Integer;
      pragma Warnings (Off, Status);
   begin
      for Index in N.List_Childs'Range loop
         Status := Eval (N.List_Childs (Index));
      end loop;

      return Status;
   end Eval_List;

   ---------------
   -- Eval_Pipe --
   ---------------

   function Eval_Pipe (N : Node) return Integer is
      Status : Integer;
      pragma Warnings (Off, Status);
   begin
      if N.Pipe_Right /= Null_Node then
         Set_Pipe_Out;
         Status := Eval (N.Pipe_Left);
         Set_Pipe_In;
         Status := Eval (N.Pipe_Right);
         Close_Pipe;
      else
         Status := Eval (N.Pipe_Left);
      end if;

      if N.Pipe_Negation then
         if Status /= 0 then
            Status := 0;
         else
            Status := 1;
         end if;
         Save_Last_Exit_Status (Status);
      end if;
      return Status;
   end Eval_Pipe;

   -------------------
   -- Eval_Subshell --
   -------------------

   function Eval_Subshell (N : Node) return Integer is
      Exit_Status : Integer := 0;
      Current_Dir : constant String := Get_Current_Dir;
   begin
      Enter_Scope;
      Push_Redirections (N.Redirections);
      begin
         Exit_Status := Eval (N.Subshell_Code);
      exception
         when Shell_Return_Exception =>
            --  A return outside of a function or a sourced script
            --  is not legitimate.
            Error ("return: can only `return'"
                   & " from a function or sourced script");
            Exit_Status := 1;
         when Shell_Exit_Exception =>
            Exit_Status := Get_Last_Exit_Status;
      end;
      Change_Dir (Current_Dir);
      Pop_Redirections;
      Leave_Scope;
      Save_Last_Exit_Status (Exit_Status);
      return Exit_Status;
   end Eval_Subshell;

   ----------------------
   -- Eval_Until_While --
   ----------------------

   function Eval_Until_While (N : Node; Is_Until : Boolean) return Integer is
      Status : Integer := 0;
      Is_Valid : Boolean;
      Break_Number : Integer;
   begin
      Push_Redirections (N.Redirections);
      loop
         begin
            if not Is_Until then
               exit when Eval (N.While_Cond) /= Null_Node;
               Status := Eval (N.While_Code);
            else
               Status := Eval (N.Until_Code);
               exit when Eval (N.Until_Cond) = 0;
            end if;
         exception
            when Continue_Exception => null;
            when E : Break_Exception =>
               To_Integer (Exception_Message (E), Break_Number, Is_Valid);
               if Break_Number = 1 then
                  exit;
               else
                  Pop_Redirections;
                  raise Break_Exception with To_String (Break_Number - 1);
               end if;
         end;
      end loop;

      Pop_Redirections;
      return Status;
   end Eval_Until_While;

end Posix_Shell.Tree.Evals;
