------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                             Posix_Shell.Opts                             --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;
with Posix_Shell.Builtins; use Posix_Shell.Builtins;
with Posix_Shell.Traces; use Posix_Shell.Traces;

package body Posix_Shell.Opts is

   procedure Usage;
   --  Print the usage of GSH on standard error.

   procedure Usage_Error (Msg : String);
   --  Print the usage annotated with the give error message.

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line
     (State : in out Shell_State;
      B : out Buffer_Access;
      Status : out Integer;
      Is_Interactive : out Boolean)
   is
      Arg_Number : Positive := 1;

      Has_Command_String : Boolean := False;
      --  Set to true if the first positional argument is the string to be
      --  parsed

      Read_From_Stdin : Boolean := False;

      Is_Login_Shell : Boolean := False;
      --  when true gshrc will be sourced

      GSHRC_Status : Integer := 0;
      pragma Unreferenced (GSHRC_Status);
   begin
      Is_Interactive := False;
      Status := 0;

      --  First, process all switches.

      while Arg_Number <= Argument_Count loop
         declare
            Arg : constant String := Argument (Arg_Number);
         begin
            --  Stop treating this argument as a switch if it does not
            --  start with a dash.
            exit when Arg'Length = 0 or else Arg (Arg'First) /= '-';

            --  Now that we know that this argument is a switch, we can
            --  "swallow it" now. That way we don't have to do it later
            --  at each exit point of this loop in addition to after
            --  having processed the switch.
            Arg_Number := Arg_Number + 1;

            if Arg = "-n" then
               Do_Script_Evaluation := False;
               Dump_Node_Table := True;
            elsif Arg = "--debug-lexer" then
               Debug_Lexer := True;
            elsif Arg = "--enable-traces" then
               Enable_Traces := True;
            elsif Arg = "-x" then
               Set_Xtrace (State, True);
            elsif Arg = "-e" then
               null;
            elsif Arg = "-ec" then
               Has_Command_String := True;
            elsif Arg = "-c" then
               Has_Command_String := True;
            elsif Arg = "-i" then
               Is_Interactive := True;
               Is_Login_Shell := True;
            elsif Arg = "--login" then
               Is_Login_Shell := True;
            elsif Arg = "-s" then
               Read_From_Stdin := True;
            elsif Arg = "--" then
               exit;
            else --  Unknown switch
               Usage_Error ("Unknown switch """ & Arg & """");
               Status := 2;
               return;
            end if;
         end;
      end loop;

      if Read_From_Stdin and Has_Command_String then
         --  We cannot read script both from stdin and from first parameter
         Usage_Error ("cannot pass both -s and -c");
         Status := 2;
         return;
      end if;

      --  Handle the case in which no positional parameter is passed to the
      --  shell
      if Arg_Number > Argument_Count then
         if Has_Command_String then
            Usage_Error ("-c option requires an argument");
            Status := 2;
            return;
         end if;

         --  We don't have any script filename pass has argument so script
         --  should be read from stdin
         Read_From_Stdin := True;
      end if;

      --  Read the buffer
      B := new Token_Buffer;
      if Has_Command_String then
         B.all := New_Buffer (Argument (Arg_Number));
         Arg_Number := Arg_Number + 1;
         if Arg_Number <= Argument_Count then
            Set_Script_Name (State, Argument (Arg_Number));
            Arg_Number := Arg_Number + 1;
         end if;
      elsif Read_From_Stdin then
         if not Is_Interactive then
            declare
               use Ada.Strings.Unbounded;
               use GNAT.OS_Lib;
               Result : Unbounded_String := To_Unbounded_String ("");
               Buffer : String (1 .. 4096);
               Buffer_Size : constant Integer := 4096;
               N : Integer;
            begin
               loop
                  N := Read (Standin, Buffer'Address, Buffer_Size);
                  Append (Result, Buffer (1 .. N));
                  exit when N < Buffer_Size;
               end loop;
               B.all := New_Buffer (To_String (Result));
            end;
         end if;
         Set_Script_Name (State, Command_Name);
      else
         begin
            B.all := New_Buffer_From_File (Argument (Arg_Number));
         exception
            when Buffer_Read_Error =>
               Put_Line (Standard_Error,
                         Argument (Arg_Number) &
                         ": no such file or directory");
               Status := 127;
               return;
         end;

         Set_Script_Name (State, Argument (Arg_Number));
         Arg_Number := Arg_Number + 1;
      end if;

      --  We have the buffer. The last thing to do is to set correctly the
      --  positional arguments

      if Arg_Number > Argument_Count then
         --  no positional parameters
         declare
            Empty_List : String_List (1 .. 0);
         begin
            Set_Positional_Parameters (State, Empty_List);
         end;
      else
         declare
            Args : String_List (1 .. Argument_Count - Arg_Number + 1);
         begin
            for J in Arg_Number .. Argument_Count loop
               Args (J - Arg_Number + 1) := new String'(Argument (J));
            end loop;
            Set_Positional_Parameters (State, Args);
         end;
      end if;

      if Is_Login_Shell then
         GSHRC_Status := Execute_Builtin
           (State, ".",
            (1 => new String'(Command_Name & "/../../etc/gsh_profile")));
      end if;

   end Process_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure P (S : String);
      --  Print S on Standard_Error;
      procedure P (S : String) is
      begin
         Put_Line (Standard_Error, S);
      end P;
   begin
      P ("Usage: gsh [options] script_name [script_parameters]");
      P ("Available options:");
      P ("    -n: Parse the script only, and dump the internal node tree");
      P ("        (do not evaluate the script).");
      P ("    -x: Turn traces on. All commands executed by the shell are");
      P ("        first printed to Standard Input.");
      P ("    --: End of options processing. Any argument after is treated");
      P ("        as filename and script_parameters");
   end Usage;

   -----------------
   -- Usage_Error --
   -----------------

   procedure Usage_Error (Msg : String) is
   begin
      Put_Line (Standard_Error, "Error: " & Msg);
      Usage;
   end Usage_Error;

end Posix_Shell.Opts;
