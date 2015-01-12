------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Command                       --
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

with Posix_Shell.Commands_Preprocessor; use Posix_Shell.Commands_Preprocessor;
with Posix_Shell.Utils;                 use Posix_Shell.Utils;
with Posix_Shell.Variables.Output;      use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Command is

   function Identify
     (S            : in out Shell_State;
      Command_Name : String;
      From         : String;
      Path_Only    : Boolean := True) return Boolean;

   function Common_to_Type_and_Which
     (S    : in out Shell_State;
      Args : String_List;
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
     (S : in out Shell_State; Args : String_List) return Integer
   is

      From : constant String := "command";
      Using_PATH     : Boolean := False;
      Invoke_Command : Boolean := True;
      Path_Only      : Boolean := False;
      Got_Errors     : Boolean := False;

      Command_List_Start : Integer := Args'First;
   begin

      --  Parse options
      for Index in Args'Range loop

         if Args (Index) (Args (Index)'First) = '-' then
            if Args (Index).all = "--" then
               Command_List_Start := Index + 1;
               exit;
            elsif Args (Index).all = "-" then
               Command_List_Start := Index;
               exit;
            end if;

            for C in Args (Index).all'First + 1 .. Args (Index).all'Last loop
               case Args (Index).all (C) is
                  when 'p'       =>
                     Using_PATH := True;
                     pragma Warnings (Off, Using_PATH);
                     --  ??? The following is 'temporary' code
                     --  not fully sure about how option '-p' should be
                     --  treated
                     Error (S,
                            From & ": -p option is currently not handled");
                     return 1;

                  when 'v' | 'V' =>
                     Invoke_Command := False;
                     Path_Only      := Args (Index).all (C) = 'v';
                  when others =>
                     Error (S,
                            From & ": unknown option: " & Args (Index).all);
                     return 1;
               end case;
            end loop;
         else
            Command_List_Start := Index;
            exit;
         end if;
      end loop;

      if Invoke_Command then

         declare
            Command   : constant String := Args (Command_List_Start).all;
            Arguments : constant String_List :=
              Args (Command_List_Start + 1 .. Args'Last);
            Result    : Integer;
         begin
            Result := Run (S, Command, Arguments, Get_Environment (S));
            return Result;
         end;

      else
         for Index in Command_List_Start .. Args'Last loop
               Got_Errors := Got_Errors and Identify (S,
                                                      Args (Index).all,
                                                      From,
                                                      Path_Only);
         end loop;
      end if;

      return Boolean'Pos (Got_Errors);

   end Command_Builtin;

   ------------------------------
   -- Common_to_Type_and_Which --
   ------------------------------

   function Common_to_Type_and_Which
     (S         : in out Shell_State;
      Args      : String_List;
      From      : String;
      Path_Only : Boolean := True) return Integer
   is
      Got_Errors : Boolean := False;
   begin

      --  No options are expected
      for Index in Args'Range loop
         if Args (Index) (Args (Index)'First) = '-' then
            Error (S,
                   From & ": unknown option: " & Args (Index).all);
            return 1;
         end if;
      end loop;

      --  Iterate on commands
      for Index in Args'Range loop
         Got_Errors := Got_Errors and Identify (S,
                                                Args (Index).all,
                                                From,
                                                Path_Only);
      end loop;

      return Boolean'Pos (Got_Errors);
   end Common_to_Type_and_Which;

   ------------------
   -- Type_Builtin --
   ------------------

   function Type_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      From       : constant String := "type";
   begin
      return Common_to_Type_and_Which (S, Args, From, False);
   end Type_Builtin;

   -------------------
   -- Which_builtin --
   -------------------

   function Which_Builtin
     (S : in out Shell_State; Args : String_List) return Integer
   is
      From       : constant String := "which";
   begin
      return Common_to_Type_and_Which (S, Args, From);
   end Which_Builtin;

end Posix_Shell.Builtins.Command;
