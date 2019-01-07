------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Command                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2019, AdaCore                   --
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

with Sh.Commands;              use Sh.Commands;
with Sh.Utils;                 use Sh.Utils;
with Sh.States.IO;      use Sh.States.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Sh.Builtins.Command is

   function Identify
     (S            : in out Shell_State;
      Command_Name : String;
      From         : String;
      Path_Only    : Boolean := True) return Boolean;

   function Common_to_Type_and_Which
     (S    : in out Shell_State;
      Args : CList;
      From : String;
      Path_Only : Boolean := True) return Integer;

   --------------
   -- Identify --
   --------------

   function Identify
     (S            : in out Shell_State;
      Command_Name : String;
      From         : String;
      Path_Only    : Boolean := True) return Boolean is

      Got_Error : Boolean := False;
      Exec_Path : String_Access;

   begin

      if Is_Builtin (Command_Name) then
         if Path_Only then
            Put (S, 1, Command_Name & ": shell built-in command");
         else
            Put (S, 1, Command_Name & " is a shell builtin");
         end if;
         New_Line (S, 1);
      elsif Is_Function (S, Command_Name) then
         Put (S, 1, Command_Name & " is a function");
         New_Line (S, 1);
      else
         Exec_Path := Locate_Exec (S, Command_Name);
         if Exec_Path = null then
            if not (From = "command" and Path_Only) then
               Error (S, Command_Name & " not found");
               Got_Error := True;
            end if;
         else
            if Path_Only then
               Put (S, 1, Exec_Path.all);
            else
               Put (S, 1, Command_Name & " is " & Exec_Path.all);
            end if;
            New_Line (S, 1);
         end if;
      end if;

      return Got_Error;
   end Identify;

   ---------------------
   -- Command_Builtin --
   ---------------------

   function Command_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is

      From : constant String := "command";
      Using_PATH     : Boolean := False;
      Invoke_Command : Boolean := True;
      Path_Only      : Boolean := False;
      Got_Errors     : Boolean := False;

      Command_List_Start : Integer := 1;
   begin

      --  Parse options
      for Index in 1 .. Length (Args) loop
         declare
            Arg : constant String := Element (Args, Index);
         begin
            if Arg (Arg'First) = '-' then
               if Arg = "--" then
                  Command_List_Start := Index + 1;
                  exit;
               elsif Arg = "-" then
                  Command_List_Start := Index;
                  exit;
               end if;

               for C in Arg'First + 1 .. Arg'Last loop
                  case Arg (C) is
                     when 'p'       =>
                        Using_PATH := True;
                        pragma Warnings (Off, Using_PATH);
                        --  ??? The following is 'temporary' code
                        --  not fully sure about how option '-p' should be
                        --  treated
                        Error (S,
                               From & ": -p option is currently not handled");
                        return (RESULT_STD, 1);

                     when 'v' | 'V' =>
                        Invoke_Command := False;
                        Path_Only      := Arg (C) = 'v';
                     when others =>
                        Error (S,
                               From & ": unknown option: " & Arg);
                        return (RESULT_STD, 1);
                  end case;
               end loop;
            else
               Command_List_Start := Index;
               exit;
            end if;
         end;
      end loop;

      if Invoke_Command then

         declare
            Command   : constant String := Element (Args, Command_List_Start);
            Arguments : CList;
            Env       : CList;
            Result    : Eval_Result;
         begin
            for Idx in Command_List_Start .. Length (Args) loop
               Append (Arguments, Element (Args, Idx));
            end loop;
            Get_Environment (S, Env);
            Result := Run (S, Command, Arguments, Env);
            Deallocate (Env);
            Deallocate (Arguments);
            case Result.Kind is
               when RESULT_STD =>
                  return (RESULT_STD, Result.Status);
               when others =>
                  return (RESULT_STD, 1);
            end case;
         end;

      else
         for Index in Command_List_Start .. Length (Args) loop
               Got_Errors := Got_Errors and Identify (S,
                                                      Element (Args, Index),
                                                      From,
                                                      Path_Only);
         end loop;
      end if;

      return (RESULT_STD,
              Boolean'Pos (Got_Errors));

   end Command_Builtin;

   ------------------------------
   -- Common_to_Type_and_Which --
   ------------------------------

   function Common_to_Type_and_Which
     (S         : in out Shell_State;
      Args      : CList;
      From      : String;
      Path_Only : Boolean := True) return Integer
   is
      Got_Errors : Boolean := False;
   begin

      --  No options are expected
      for Index in 1 .. Length (Args) loop
         declare
            Arg : constant String := Element (Args, Index);
         begin
            if Arg (Arg'First) = '-' then
               Error (S,
                      From & ": unknown option: " & Arg);
               return 1;
            end if;
         end;
      end loop;

      --  Iterate on commands
      for Index in 1 .. Length (Args) loop
         Got_Errors := Got_Errors and Identify (S,
                                                Element (Args, Index),
                                                From,
                                                Path_Only);
      end loop;

      return Boolean'Pos (Got_Errors);
   end Common_to_Type_and_Which;

   ------------------
   -- Type_Builtin --
   ------------------

   function Type_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      From       : constant String := "type";
   begin
      return (RESULT_STD, Common_to_Type_and_Which (S, Args, From, False));
   end Type_Builtin;

   -------------------
   -- Which_builtin --
   -------------------

   function Which_Builtin
     (S : in out Shell_State; Args : CList) return Eval_Result
   is
      From       : constant String := "which";
   begin
      return (RESULT_STD, Common_to_Type_and_Which (S, Args, From));
   end Which_Builtin;

end Sh.Builtins.Command;
