------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Builtins                           --
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

with Ada.Strings.Unbounded;

with Posix_Shell.Annotated_Strings;     use Posix_Shell.Annotated_Strings;
with Posix_Shell.Builtins.Basename;     use Posix_Shell.Builtins.Basename;
with Posix_Shell.Builtins.Booleans;     use Posix_Shell.Builtins.Booleans;
with Posix_Shell.Builtins.Cat;          use Posix_Shell.Builtins.Cat;
with Posix_Shell.Builtins.Cd;           use Posix_Shell.Builtins.Cd;
with Posix_Shell.Builtins.Command;      use Posix_Shell.Builtins.Command;
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
with Posix_Shell.Builtins.Uname;        use Posix_Shell.Builtins.Uname;
with Posix_Shell.Builtins.Wc;           use Posix_Shell.Builtins.Wc;

with Posix_Shell.Commands;              use Posix_Shell.Commands;
with Posix_Shell.Exec;                  use Posix_Shell.Exec;
with Posix_Shell.Parser;                use Posix_Shell.Parser;
with Posix_Shell.String_Utils;          use Posix_Shell.String_Utils;
with Posix_Shell.Subst;                 use Posix_Shell.Subst;
with Posix_Shell.Tree;                  use Posix_Shell.Tree;
with Posix_Shell.Tree.Evals;            use Posix_Shell.Tree.Evals;
with Posix_Shell.Utils;                 use Posix_Shell.Utils;
with Posix_Shell.Variables.Output;      use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins is

   type Builtin_Function
     is access function
       (S : in out Shell_State; Args : String_List) return Integer;
   --  The signature of a function implementing a given builtin.

   function Get_Builtin (Name : String) return Builtin_Function;
   --  Return function associated with a builtin. If there is no builtin
   --  called Name then null is returned.

   function Return_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;
   --  Implement the "return" builtin.

   function Shift_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;
   --  Implement the "shift" builtin.

   function Exec_Builtin
     (S : in out Shell_State; Args : String_List)
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
     (S : in out Shell_State; Args : String_List) return Integer;

   function Trap_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Export_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Set_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Unsetenv_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Exit_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Continue_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Break_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Read_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   function Unset_Builtin
     (S : in out Shell_State; Args : String_List) return Integer;

   -------------------
   -- Break_Builtin --
   -------------------

   function Break_Builtin
     (S : in out Shell_State; Args : String_List)
      return Integer
   is
      Break_Number : Integer;
      Is_Valid : Boolean;
      Current_Loop_Level : constant Natural := Get_Loop_Scope_Level (S);
   begin

      if Current_Loop_Level = 0 then
         --  we are not in a loop construct. So ignore the builtin and return 0
         Put (S, 2, "break: only meaningful in a " &
              "`for', `while', or `until' loop" & ASCII.LF);
         return 0;
      end if;

      if Args'Length = 0 then
         Break_Number := Current_Loop_Level;
      else
         To_Integer (Args (Args'First).all, Break_Number, Is_Valid);
         if not Is_Valid then
            Put (S, 2, "break: " &
                 Args (Args'First).all &
                 ":numeric argument required" & ASCII.LF);
            Shell_Exit (S, 128);
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
     (S : in out Shell_State;
      Args : String_List)
      return Integer
   is
      Break_Number : Integer;
      Is_Valid : Boolean;
      Current_Loop_Level : constant Natural := Get_Loop_Scope_Level (S);
   begin

      if Current_Loop_Level = 0 then
         --  we are not in a loop construct. So ignore the builtin and return 0
         Put (S, 2, "continue: only meaningful in a " &
              "`for', `while', or `until' loop" & ASCII.LF);
         return 0;
      end if;

      if Args'Length = 0 then
         Break_Number := Current_Loop_Level;
      else
         To_Integer (Args (Args'First).all, Break_Number, Is_Valid);
         if not Is_Valid then
            Put (S, 2, "continue: " &
                 Args (Args'First).all &
                 ":numeric argument required" & ASCII.LF);
            Shell_Exit (S, 128);
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
     (S : in out Shell_State; Args : String_List) return Integer
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
      return Get_Last_Exit_Status (S);
   exception
      when Shell_Syntax_Error =>
         return 1;
   end Eval_Builtin;

   ------------------
   -- Exec_Builtin --
   ------------------

   function Exec_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
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
            Result := Run (S, Command, Arguments, Get_Environment (S));
            Shell_Exit (S, Result);
         end;
      end if;
      return 0;
   end Exec_Builtin;

   ---------------------
   -- Execute_Builtin --
   ---------------------

   function Execute_Builtin
     (S    : in out Shell_State;
      Cmd  : String;
      Args : String_List)
      return Integer
   is
      Builtin : constant Builtin_Function := Get_Builtin (Cmd);
   begin
      return Builtin (S, Args);
   end Execute_Builtin;

   ------------------
   -- Exit_Builtin --
   ------------------

   function Exit_Builtin
     (S    : in out Shell_State;
      Args : String_List)
      return Integer
   is
      Exit_Code : Integer;
      Success : Boolean;
   begin
      --  Calling "exit" without argument is equivalent to calling
      --  it with "$?" as its argument.
      if Args'Length = 0 then
         Shell_Exit (S, Get_Last_Exit_Status (S));
      end if;

      --  If more than one argument was provided, print an error
      --  message and return 1.
      if Args'Length > 1 then
         Error (S, "exit: too many arguments");
         return 1;
      end if;

      To_Integer (Args (Args'First).all, Exit_Code, Success);
      if not Success then
         --  The exit code value is not entirely numeric.
         --  Report the error and exit with value 255.
         Error (S, "exit: " & Args (Args'First).all
                & ": numeric argument required");
         Exit_Code := 255;
      end if;

      --  Exit with the code specified.
      Shell_Exit (S, Exit_Code);
   end Exit_Builtin;

   ------------
   -- Export --
   ------------

   function Export_Builtin
     (S : in out Shell_State;
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
               Set_Var_Value (S,
                              Arg (Arg'First .. Equal_Pos - 1),
                              Arg (Equal_Pos + 1 .. Arg'Last),
                              Export => True);
            else
               Export_Var (S, Arg);
            end if;
         end;
      end loop;
      return 0;
   end Export_Builtin;

   -----------------
   -- Get_Builtin --
   -----------------

   function Get_Builtin (Name : String) return Builtin_Function
   is
      L : constant Natural := Name'Length;
   begin
      if L = 0 then
         return null;
      end if;

      --  The case statement is divided in two part. First part handle
      --  builtins that are working for all platforms and the second part the
      --  builtins that are present only on the windows platform

      case Name (Name'First) is
         when '.' =>
            if L = 1 then
               return Source_Builtin'Access;
            end if;

         when ':' =>
            if L = 1 then
               return True_Builtin'Access;
            end if;

         when '[' =>
            if L = 1 then
               return Test_Builtin'Access;
            end if;

         when 'b' =>
            if L = 8 and then Name = "basename" then
               return Basename_Builtin'Access;
            elsif L = 5 and then Name = "break" then
               return Break_Builtin'Access;
            end if;

         when 'c' =>
            if  L = 3 and then Name = "cat" then
               return Cat_Builtin'Access;
            elsif L = 2 and then Name = "cd" then
               return Change_Dir_Builtin'Access;
            elsif L = 7 and then Name = "command" then
               return Command_Builtin'Access;
            elsif L = 8 and then Name = "continue" then
               return Continue_Builtin'Access;
            elsif L = 2 and then Name = "cp" then
               return Cp_Builtin'Access;
            end if;

         when 'd' =>
            if L = 7 and then Name = "dirname" then
               return Dirname_Builtin'Access;
            end if;

         when 'e' =>
            if L = 4 then
               if Name = "echo" then
                  return Echo_Builtin'Access;
               elsif Name = "eval" then
                  return Eval_Builtin'Access;
               elsif Name = "exec" then
                  return Exec_Builtin'Access;
               elsif Name = "exit" then
                  return Exit_Builtin'Access;
               elsif Name = "expr" then
                  return Expr_Builtin'Access;
               end if;
            elsif L = 6 and then Name = "export" then
               return Export_Builtin'Access;
            end if;

         when 'f' =>
            if L = 5 and then Name = "false" then
               return False_Builtin'Access;
            end if;

         when 'h' =>
            if L = 4 and then Name = "head" then
               return Head_Builtin'Access;
            end if;

         when 'l' =>
            if L = 5 and then Name = "limit" then
               return Limit_Builtin'Access;
            end if;

         when 'm' =>
            if L = 5 and then Name = "mkdir" then
               return Mkdir_Builtin'Access;
            end if;

         when 'p' =>
            if L = 6 and then Name = "printf" then
               return Printf_Builtin'Access;
            elsif L = 3 and then Name = "pwd" then
               return Pwd_Builtin'Access;
            end if;

         when 'r' =>
            if L = 4 and then Name = "read" then
               return Read_Builtin'Access;
            elsif L = 5 and then Name = "recho" then
               return REcho_Builtin'Access;
            elsif L = 6 and then Name = "return" then
               return Return_Builtin'Access;
            end if;
         when 's' =>
            if L = 3 and then Name = "set" then
               return Set_Builtin'Access;
            elsif L = 6  and then Name = "setenv" then
               return Export_Builtin'Access;
            elsif L = 5 and then Name = "shift" then
               return Shift_Builtin'Access;
            end if;

         when 't' =>
            if L = 4 then
               if Name = "tail" then
                  return Tail_Builtin'Access;
               elsif Name = "test" then
                  return Test_Builtin'Access;
               elsif Name = "trap" then
                  return Trap_Builtin'Access;
               elsif Name = "true" then
                  return True_Builtin'Access;
               elsif Name = "type" then
                  return Type_Builtin'Access;
               end if;
            end if;

         when 'u' =>
            if L = 5 and then Name = "umask" then
               return Limit_Builtin'Access;
            elsif L = 5 and then Name = "unset" then
               return Unset_Builtin'Access;
            elsif L = 8 and then Name = "unsetenv" then
               return Unsetenv_Builtin'Access;
            end if;
         when 'w' =>
            if L = 2 and then Name = "wc" then
               return Wc_Builtin'Access;
            elsif L = 5 and then Name = "which" then
               return Which_Builtin'Access;
            end if;
         when others =>
            null;
      end case;

      if Is_Windows then
         case Name (Name'First) is
            when 'r' =>
               if L = 2 and then Name = "rm" then
                  return Rm_Builtin'Access;
               end if;

            when 'u' =>
               if L = 5 and then Name = "uname" then
                  return Uname_Builtin'Access;
               end if;

            when others =>
               null;
         end case;
      end if;

      return null;
   end Get_Builtin;

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Cmd : String) return Boolean is
   begin
      return Get_Builtin (Cmd) /= null;
   end Is_Builtin;

   ------------------
   -- Read_Builtin --
   ------------------

   function Read_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      CC : Character := Read (S, 0);
      Line : Annotated_String;
   begin
      --  First read the complete line
      while CC /= ASCII.LF and CC /= ASCII.EOT loop
         if CC /= ASCII.CR then
            Append (Line, CC);
         end if;
         CC := Read (S, 0);
      end loop;

      if Args'Length > 0 then
         declare
            List : constant String_List := Split_String
              (S, Str (Line), Args'Length - 1);
            Index : Integer := List'First;
         begin

            for J in Args'Range loop
               if Index <= List'Last then
                  Set_Var_Value (S, Args (J).all, List (Index).all);
                  Index := Index + 1;
               else
                  Set_Var_Value (S, Args (J).all, "");
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

   --------------------
   -- Return_Builtin --
   --------------------

   function Return_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      Return_Value : Integer;
      Success : Boolean;
   begin
      --  If more than one argument was provided: Report an error and
      --  execute the return with a fixed return value of 1.
      if Args'Length > 1 then
         Error (S, "return: too many arguments");
         Save_Last_Exit_Status (S, 1);

      --  If one argument was provided: Return with this value.
      elsif Args'Length = 1 then
         To_Integer (Args (Args'First).all, Return_Value, Success);
         if not Success then
            --  The provided value is not a valid number, so report
            --  this problem, and force the return value to 255.
            Error (S, "return: " & Args (Args'First).all
                   & ": numeric argument required");
            Return_Value := 255;
         end if;
         Save_Last_Exit_Status (S, Return_Value);
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
     (S : in out Shell_State; Args : String_List) return Integer
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
                     Set_Xtrace (S, True);
                  elsif Args (Index).all = "-f" then
                     Set_File_Expansion (S, False);
                  else
                     null;
                  end if;
               when '+' =>
                  if Args (Index).all = "+x" then
                     Set_Xtrace (S, False);
                  elsif Args (Index).all = "+f" then
                     Set_File_Expansion (S, True);
                  end if;

               when others => Saved_Index := Index; exit;
            end case;
         end if;
      end loop;

      if Saved_Index >= Args'First and then Saved_Index <= Args'Last then
         Set_Positional_Parameters (S, Args (Saved_Index .. Args'Last));
      end if;
      return 0;
   end Set_Builtin;

   -------------------
   -- Shift_Builtin --
   -------------------

   function Shift_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      Shift : Integer := 1;
      Success : Boolean;
   begin
      --  If more than one argument was provided: Report an error,
      --  and abort with error code 1.

      if Args'Length > 1 then
         Error (S, "shift: too many arguments");
         Shell_Exit (S, 1);
      end if;

      --  If one argument was provided, convert it to an integer
      --  and use it as the number of shifts to perform.

      if Args'Length = 1 then
         To_Integer (Args (Args'First).all, Shift, Success);

         if not Success then
            Error (S, "shift: " & Args (Args'First).all
                   & ": numeric argument required");
            Shell_Exit (S, 1);
         end if;

         if Shift < 0 then
            Error (S, "shift: " & Args (Args'First).all
                   & ": shift count out of range");
            return 1;
         end if;
      end if;

      Shift_Positional_Parameters (S, Shift, Success);
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
     (S : in out Shell_State; Args : String_List) return Integer
   is

   begin
      if Args'Length = 0 then
         --  To be implemented (should show the list of registered actions)
         null;
         return 0;
      elsif Args'Length = 1 then
         Error (S, "trap: invalid number of arguments");
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
                  Set_Trap_Action (S, null, Signal_Number);
               else
                  Set_Trap_Action (S,
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
     (S : in out Shell_State; Args : String_List) return Integer
   is
   begin
      for Index in Args'Range loop
         Unset_Var (S, Args (Index).all);
      end loop;
      return 0;
   end Unset_Builtin;

   ----------------------
   -- Unsetenv_Builtin --
   ----------------------

   function Unsetenv_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
   begin
      Export_Var (S, Args (Args'First).all, "");
      return 0;
   end Unsetenv_Builtin;

end Posix_Shell.Builtins;
