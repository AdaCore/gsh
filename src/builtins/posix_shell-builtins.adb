------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Builtins                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2014, AdaCore                   --
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

with Posix_Shell.Annotated_Strings;     use Posix_Shell.Annotated_Strings;
with Posix_Shell.Builtins.Basename;     use Posix_Shell.Builtins.Basename;
with Posix_Shell.Builtins.Booleans;     use Posix_Shell.Builtins.Booleans;
with Posix_Shell.Builtins.Cat;          use Posix_Shell.Builtins.Cat;
with Posix_Shell.Builtins.Cd;           use Posix_Shell.Builtins.Cd;
with Posix_Shell.Builtins.Cp;           use Posix_Shell.Builtins.Cp;
with Posix_Shell.Builtins.Dirname;      use Posix_Shell.Builtins.Dirname;
with Posix_Shell.Builtins.Echo;         use Posix_Shell.Builtins.Echo;
with Posix_Shell.Builtins.Expr;         use Posix_Shell.Builtins.Expr;
with Posix_Shell.Builtins.Head;         use Posix_Shell.Builtins.Head;
with Posix_Shell.Builtins.Limit;        use Posix_Shell.Builtins.Limit;
with Posix_Shell.Builtins.Mkdir;        use Posix_Shell.Builtins.Mkdir;
with Posix_Shell.Builtins.Printf;       use Posix_Shell.Builtins.Printf;
with Posix_Shell.Builtins.Pwd;          use Posix_Shell.Builtins.Pwd;
with Posix_Shell.Builtins.Rm;           use Posix_Shell.Builtins.Rm;
with Posix_Shell.Builtins.Source;       use Posix_Shell.Builtins.Source;
with Posix_Shell.Builtins.Tail;         use Posix_Shell.Builtins.Tail;
with Posix_Shell.Builtins.Test;         use Posix_Shell.Builtins.Test;
with Posix_Shell.Builtins.Uname;         use Posix_Shell.Builtins.Uname;
with Posix_Shell.Builtins.Wc;           use Posix_Shell.Builtins.Wc;
with Posix_Shell.Commands_Preprocessor; use Posix_Shell.Commands_Preprocessor;
with Posix_Shell.Exec;                  use Posix_Shell.Exec;
with Posix_Shell.Functions;             use Posix_Shell.Functions;
with Posix_Shell.Parser;                use Posix_Shell.Parser;
with Posix_Shell.String_Utils;          use Posix_Shell.String_Utils;
with Posix_Shell.Subst;                 use Posix_Shell.Subst;
with Posix_Shell.Tree;                  use Posix_Shell.Tree;
with Posix_Shell.Tree.Evals;            use Posix_Shell.Tree.Evals;
with Posix_Shell.Utils;                 use Posix_Shell.Utils;
with Posix_Shell.Variables.Output;      use Posix_Shell.Variables.Output;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package body Posix_Shell.Builtins is

   type Builtin_Function
     is access function
       (S : Shell_State_Access; Args : String_List) return Integer;
   --  The signature of a function implementing a given builtin.

   package Builtin_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String,
        Builtin_Function,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Builtin_Maps;

   Builtin_Map : Map;
   --  A map of all builtins.

   function Return_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;
   --  Implement the "return" builtin.

   function Shift_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;
   --  Implement the "shift" builtin.

   function Exec_Builtin
     (S : Shell_State_Access; Args : String_List)
      return Integer;
   --  The exec utility shall open, close, and/or copy file descriptors as
   --  specified by any redirections as part of the command.
   --  If exec is specified with command, it shall replace the shell with
   --  command without creating a new process. If arguments are specified, they
   --  shall be arguments to command. Redirection affects the current shell
   --  execution environment. (part of POSIX.1-2008).
   --
   --  Implementation Notes: a special case in Posix_Shell_Tree.Evals.Eval_Cmd
   --  is taking care of exec redirections. Furthermore in case a command is
   --  specified to exec a process will be spawned as there is no notion of
   --  fork/exec on Windows systems.

   function Eval_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Trap_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Export_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Set_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Unsetenv_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Exit_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Continue_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Break_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Type_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Read_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   function Unset_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer;

   -------------------
   -- Break_Builtin --
   -------------------

   function Break_Builtin
     (S : Shell_State_Access; Args : String_List)
      return Integer
   is
      Break_Number : Integer;
      Is_Valid : Boolean;
      Current_Loop_Level : constant Natural := Get_Loop_Scope_Level (S.all);
   begin

      if Current_Loop_Level = 0 then
         --  we are not in a loop construct. So ignore the builtin and return 0
         Put (S.all, 2, "break: only meaningful in a " &
              "`for', `while', or `until' loop" & ASCII.LF);
         return 0;
      end if;

      if Args'Length = 0 then
         Break_Number := Current_Loop_Level;
      else
         To_Integer (Args (Args'First).all, Break_Number, Is_Valid);
         if not Is_Valid then
            Put (S.all, 2, "break: " &
                 Args (Args'First).all &
                 ":numeric argument required" & ASCII.LF);
            Shell_Exit (S.all, 128);
         end if;

         if Break_Number < 1 then
            return 1;
         end if;

         Break_Number := Current_Loop_Level - Break_Number + 1;
         if Break_Number < 1 then
            Break_Number := 1;
         end if;
      end if;

      raise Break_Exception with To_String (Break_Number);

      return 0;
   end Break_Builtin;

   ----------------------
   -- Continue_Builtin --
   ----------------------

   function Continue_Builtin
     (S : Shell_State_Access;
      Args : String_List)
      return Integer
   is
      Break_Number : Integer;
      Is_Valid : Boolean;
      Current_Loop_Level : constant Natural := Get_Loop_Scope_Level (S.all);
   begin

      if Current_Loop_Level = 0 then
         --  we are not in a loop construct. So ignore the builtin and return 0
         Put (S.all, 2, "continue: only meaningful in a " &
              "`for', `while', or `until' loop" & ASCII.LF);
         return 0;
      end if;

      if Args'Length = 0 then
         Break_Number := Current_Loop_Level;
      else
         To_Integer (Args (Args'First).all, Break_Number, Is_Valid);
         if not Is_Valid then
            Put (S.all, 2, "continue: " &
                 Args (Args'First).all &
                 ":numeric argument required" & ASCII.LF);
            Shell_Exit (S.all, 128);
         end if;

         if Break_Number < 1 then
            return 1;
         end if;

         Break_Number := Current_Loop_Level - Break_Number + 1;
         if Break_Number < 1 then
            Break_Number := 1;
         end if;
      end if;

      raise Continue_Exception with To_String (Break_Number);

      return 0;
   end Continue_Builtin;

   ------------------
   -- Eval_Builtin --
   ------------------

   function Eval_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Str : Ada.Strings.Unbounded.Unbounded_String;
      T : Shell_Tree;
      use Ada.Strings.Unbounded;
   begin
      Str := To_Unbounded_String (Args (Args'First).all);
      for I in Args'First + 1 .. Args'Last loop
         Str := Str & " " & Args (I).all;
      end loop;

      T := Parse_String (To_String (Str));
      Eval (S, T);
      Free_Node (T);
      return Get_Last_Exit_Status (S.all);
   exception
      when Shell_Syntax_Error =>
         return 1;
   end Eval_Builtin;

   ------------------
   -- Exec_Builtin --
   ------------------

   function Exec_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Result : Integer;
   begin
      if Args'Length = 0 then
         return 0;
      else
         declare
            Command : constant String := Args (Args'First).all;
            Arguments : constant String_List :=
              Args (Args'First + 1 .. Args'Last);
         begin
            Result := Run (S, Command, Arguments, Get_Environment (S.all));
            Shell_Exit (S.all, Result);
         end;
      end if;
      return 0;
   end Exec_Builtin;

   ---------------------
   -- Execute_Builtin --
   ---------------------

   function Execute_Builtin
     (S    : Shell_State_Access;
      Cmd  : String;
      Args : String_List)
      return Integer
   is
      Builtin : constant Builtin_Function := Element (Builtin_Map, Cmd);
   begin
      return Builtin (S, Args);
   end Execute_Builtin;

   ------------------
   -- Exit_Builtin --
   ------------------

   function Exit_Builtin
     (S    : Shell_State_Access;
      Args : String_List)
      return Integer
   is
      Exit_Code : Integer;
      Success : Boolean;
   begin
      --  Calling "exit" without argument is equivalent to calling
      --  it with "$?" as its argument.
      if Args'Length = 0 then
         Shell_Exit (S.all, Get_Last_Exit_Status (S.all));
      end if;

      --  If more than one argument was provided, print an error
      --  message and return 1.
      if Args'Length > 1 then
         Error (S.all, "exit: too many arguments");
         return 1;
      end if;

      To_Integer (Args (Args'First).all, Exit_Code, Success);
      if not Success then
         --  The exit code value is not entirely numeric.
         --  Report the error and exit with value 255.
         Error (S.all, "exit: " & Args (Args'First).all
                & ": numeric argument required");
         Exit_Code := 255;
      end if;

      --  Exit with the code specified.
      Shell_Exit (S.all, Exit_Code);
   end Exit_Builtin;

   ------------
   -- Export --
   ------------

   function Export_Builtin
     (S : Shell_State_Access;
      Args : String_List)
      return Integer
   is
   begin
      for I in Args'Range loop
         declare
            Arg : constant String := Args (I).all;
            Equal_Pos : Integer := Arg'First;
         begin
            for Index in Arg'Range loop
               if Arg (Index) = '=' then
                  Equal_Pos := Index;
                  exit;
               end if;
            end loop;

            if Equal_Pos > Arg'First then
               Set_Var_Value (S.all,
                              Arg (Arg'First .. Equal_Pos - 1),
                              Arg (Equal_Pos + 1 .. Arg'Last),
                              Export => True);
            else
               Export_Var (S.all, Arg);
            end if;
         end;
      end loop;
      return 0;
   end Export_Builtin;

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Cmd : String) return Boolean is
   begin
      return Contains (Builtin_Map, Cmd);
   end Is_Builtin;

   ------------------
   -- Type_Builtin --
   ------------------

   function Type_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Exit_Status : Integer := 0;
      Exec_Path : String_Access;
   begin
      for Index in Args'Range loop
         if Is_Builtin (Args (Index).all) then
            Put (S.all, 1, Args (Index).all & " is a builtin");
            New_Line (S.all, 1);
         elsif Is_Function (Args (Index).all) then
            Put (S.all, 1, Args (Index).all & " is a function");
            New_Line (S.all, 1);
         else
            Exec_Path := Locate_Exec (S.all, Args (Index).all);
            if Exec_Path = null then
               Error (S.all, Args (Index).all & ": not found");
               Exit_Status := 1;
            else
               Put (S.all, 1, Args (Index).all & " is " & Exec_Path.all);
               New_Line (S.all, 1);
            end if;
         end if;
      end loop;
      return Exit_Status;
   end Type_Builtin;

   ------------------
   -- Read_Builtin --
   ------------------

   function Read_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      CC : Character := Read (S.all, 0);
      Line : Annotated_String := Null_Annotated_String;
   begin
      --  First read the complete line
      while CC /= ASCII.LF and CC /= ASCII.EOT loop
         if CC /= ASCII.CR then
            Append (Line, CC);
         end if;
         CC := Read (S.all, 0);
      end loop;

      if Args'Length > 0 then
         declare
            List : constant String_List := Split_String
              (S, Str (Line), Args'Length - 1);
            Index : Integer := List'First;
         begin

            for J in Args'Range loop
               if Index <= List'Last then
                  Set_Var_Value (S.all, Args (J).all, List (Index).all);
                  Index := Index + 1;
               else
                  Set_Var_Value (S.all, Args (J).all, "");
               end if;
            end loop;
         end;
      end if;

      if CC = ASCII.EOT then
         return 1;
      else
         return 0;
      end if;

   end Read_Builtin;

   -------------------------------
   -- Register_Default_Builtins --
   -------------------------------

   procedure Register_Default_Builtins is
   begin
      --  Register all the builtins.
      Include (Builtin_Map, "pwd",           Pwd_Builtin'Access);
      Include (Builtin_Map, ".",             Source_Builtin'Access);
      Include (Builtin_Map, "cd",            Change_Dir_Builtin'Access);
      Include (Builtin_Map, "echo",          Echo_Builtin'Access);
      Include (Builtin_Map, "eval",          Eval_Builtin'Access);
      Include (Builtin_Map, "exit",          Exit_Builtin'Access);
      Include (Builtin_Map, "export",        Export_Builtin'Access);
      Include (Builtin_Map, "false",         False_Builtin'Access);
      Include (Builtin_Map, "limit",         Limit_Builtin'Access);
      Include (Builtin_Map, "recho",         REcho_Builtin'Access);
      Include (Builtin_Map, "return",        Return_Builtin'Access);
      Include (Builtin_Map, "set",           Set_Builtin'Access);
      Include (Builtin_Map, "setenv",        Export_Builtin'Access);
      Include (Builtin_Map, "shift",         Shift_Builtin'Access);
      Include (Builtin_Map, "test",          Test_Builtin'Access);
      Include (Builtin_Map, "true",          True_Builtin'Access);
      Include (Builtin_Map, "umask",         Limit_Builtin'Access);
      Include (Builtin_Map, "unsetenv",      Unsetenv_Builtin'Access);
      Include (Builtin_Map, "unset",         Unset_Builtin'Access);
      Include (Builtin_Map, "[",             Test_Builtin'Access);
      Include (Builtin_Map, "printf",        Printf_Builtin'Access);
      Include (Builtin_Map, "expr",          Expr_Builtin'Access);
      Include (Builtin_Map, ":",             True_Builtin'Access);
      Include (Builtin_Map, "exec",          Exec_Builtin'Access);
      Include (Builtin_Map, "continue",      Continue_Builtin'Access);
      Include (Builtin_Map, "break",         Break_Builtin'Access);
      Include (Builtin_Map, "trap",          Trap_Builtin'Access);
      Include (Builtin_Map, "cat",           Cat_Builtin'Access);
      Include (Builtin_Map, "read",          Read_Builtin'Access);
      Include (Builtin_Map, "wc",            Wc_Builtin'Access);
      Include (Builtin_Map, "cp",            Cp_Builtin'Access);

      --  if GNAT.Directory_Operations.Dir_Separator = '\' then
         --  No need to include these builtins on non-windows machines
         Include (Builtin_Map, "tail",          Tail_Builtin'Access);
         Include (Builtin_Map, "head",          Head_Builtin'Access);
         Include (Builtin_Map, "rm",            Rm_Builtin'Access);
         Include (Builtin_Map, "type",          Type_Builtin'Access);
         Include (Builtin_Map, "basename",      Basename_Builtin'Access);
         Include (Builtin_Map, "dirname",       Dirname_Builtin'Access);
         Include (Builtin_Map, "mkdir",         Mkdir_Builtin'Access);
         Include (Builtin_Map, "uname",         Uname_Builtin'Access);
      --  end if;
   end Register_Default_Builtins;

   --------------------
   -- Return_Builtin --
   --------------------

   function Return_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Return_Value : Integer;
      Success : Boolean;
   begin
      --  If more than one argument was provided: Report an error and
      --  execute the return with a fixed return value of 1.
      if Args'Length > 1 then
         Error (S.all, "return: too many arguments");
         Save_Last_Exit_Status (S.all, 1);

      --  If one argument was provided: Return with this value.
      elsif Args'Length = 1 then
         To_Integer (Args (Args'First).all, Return_Value, Success);
         if not Success then
            --  The provided value is not a valid number, so report
            --  this problem, and force the return value to 255.
            Error (S.all, "return: " & Args (Args'First).all
                   & ": numeric argument required");
            Return_Value := 255;
         end if;
         Save_Last_Exit_Status (S.all, Return_Value);
      end if;

      --  Note: If no argument is provided, then return using the exit
      --  status of the last command executed. This exit status has
      --  already been saved, so no need to save it again.

      --  Now perform the return.
      raise Shell_Return_Exception;

      --  Ugh! The compiler insists that a "return" should be found in
      --  the function body or the compilation will fail. The following
      --  return statement is absolutely useless, but makes the compiler
      --  happy.
      return 0;
   end Return_Builtin;

   -----------------
   -- Set_Builtin --
   -----------------

   function Set_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Saved_Index : Integer := -1;
   begin
      if Args'Length = 0 then
         return 0;
      end if;

      --  Parse options
      for Index in Args'Range loop
         if Args (Index)'Length > 0 then
            case Args (Index) (Args (Index)'First) is
               when '-' =>
                  if Args (Index).all = "--" then
                     Saved_Index := Index + 1;
                     exit;
                  elsif Args (Index).all = "-x" then
                     Set_Xtrace (S.all, True);
                  elsif Args (Index).all = "-f" then
                     Set_File_Expansion (S.all, False);
                  else
                     null;
                  end if;
               when '+' =>
                  if Args (Index).all = "+x" then
                     Set_Xtrace (S.all, False);
                  elsif Args (Index).all = "+f" then
                     Set_File_Expansion (S.all, True);
                  end if;

               when others => Saved_Index := Index; exit;
            end case;
         end if;
      end loop;

      if Saved_Index >= Args'First and then Saved_Index <= Args'Last then
         Set_Positional_Parameters (S.all, Args (Saved_Index .. Args'Last));
      end if;
      return 0;
   end Set_Builtin;

   -------------------
   -- Shift_Builtin --
   -------------------

   function Shift_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Shift : Integer := 1;
      Success : Boolean;
   begin
      --  If more than one argument was provided: Report an error,
      --  and abort with error code 1.

      if Args'Length > 1 then
         Error (S.all, "shift: too many arguments");
         Shell_Exit (S.all, 1);
      end if;

      --  If one argument was provided, convert it to an integer
      --  and use it as the number of shifts to perform.

      if Args'Length = 1 then
         To_Integer (Args (Args'First).all, Shift, Success);

         if not Success then
            Error (S.all, "shift: " & Args (Args'First).all
                   & ": numeric argument required");
            Shell_Exit (S.all, 1);
         end if;

         if Shift < 0 then
            Error (S.all, "shift: " & Args (Args'First).all
                   & ": shift count out of range");
            return 1;
         end if;
      end if;

      Shift_Positional_Parameters (S.all, Shift, Success);
      if Success then
         return 0;
      else
         return 1;
      end if;
   end Shift_Builtin;

   ------------------
   -- Trap_Builtin --
   ------------------

   function Trap_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is

   begin
      if Args'Length = 0 then
         --  To be implemented (should show the list of registered actions)
         null;
         return 0;
      elsif Args'Length = 1 then
         Error (S.all, "trap: invalid number of arguments");
         return 1;
      end if;

      for J in Args'First + 1 .. Args'Last loop
         declare
            Cond : constant String := Args (J).all;
            Signal_Number : Integer;
         begin
            if Cond = "EXIT" or else Cond = "0" then
               Signal_Number := 0;
            else
               --  Just ignore other signals for the moment ...
               Signal_Number := -1;
            end if;

            if Signal_Number >= 0 then
               if Args (Args'First).all = "-" then
                  Set_Trap_Action (S.all, null, Signal_Number);
               else
                  Set_Trap_Action (S.all,
                                   new String'(Args (Args'First).all),
                                   Signal_Number);
               end if;
            end if;
         end;
      end loop;
      return 0;
   end Trap_Builtin;

   -------------------
   -- Unset_Builtin --
   -------------------

   function Unset_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
   begin
      for Index in Args'Range loop
         Unset_Var (S.all, Args (Index).all);
      end loop;
      return 0;
   end Unset_Builtin;

   ----------------------
   -- Unsetenv_Builtin --
   ----------------------

   function Unsetenv_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
   begin
      Export_Var (S.all, Args (Args'First).all, "");
      return 0;
   end Unsetenv_Builtin;

end Posix_Shell.Builtins;
