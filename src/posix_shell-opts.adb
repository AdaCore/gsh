with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Posix_Shell.Opts is

   Script_Name_Argument : Natural := 0;
   --  The index in the list of command arguments where the script
   --  name is provided. Zero means unset.

   First_Script_Argument : Natural := 0;
   --  The index of the first script argument in the command line.
   --  Zero means unset. A value that's greater than the number of
   --  total arguments means no script argument was provided.

   procedure Usage;
   --  Print the usage of GSH on standard error.

   procedure Usage_Error (Msg : String);
   --  Print the usage annotated with the give error message.

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line (Success : out Boolean) is
      Arg_Number : Positive := 1;
   begin
      Success := True;

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

            elsif Arg = "-x" then
               Debug_Mode := True;
            elsif Arg = "-e" then
               null;
            elsif Arg = "-c" then

               Run_Command := True;
            elsif Arg = "--" then
               exit;

            else --  Unknown switch
               Usage_Error ("Unknown switch """ & Arg & """");
               Success := False;
               return;
            end if;
         end;
      end loop;

      --  Now that all switches have been processed, the next argument
      --  (if present) is the name of the script to execute.

      if Arg_Number > Argument_Count then
         Usage_Error ("script name is missing");
         Success := False;
         return;
      end if;
      Script_Name_Argument := Arg_Number;

      --  All remaining arguments, if any, are arguments for the script.
      First_Script_Argument := Arg_Number + 1;

   end Process_Command_Line;

   ----------------------
   -- Script_Arguments --
   ----------------------

   function Script_Arguments return String_List is
      Nb_Args : constant Integer := Argument_Count - First_Script_Argument + 1;
   begin
      --  If no script arguments were provided then return an empty list.
      if Nb_Args < 1 then
         declare
            Empty_List : String_List (1 .. 0);
         begin
            return Empty_List;
         end;
      end if;

      --  Build the string list and then return it.
      declare
         Args : String_List (1 .. Nb_Args);
      begin
         for J in 1 .. Nb_Args loop
            Args (J) := new String'(Argument (First_Script_Argument + J - 1));
            --  Ada.Text_IO.Put_Line (Args (J).all);
         end loop;

         return Args;
      end;
   end Script_Arguments;

   -----------------
   -- Script_Name --
   -----------------

   function Script_Name return String is
   begin
      return Argument (Script_Name_Argument);
   end Script_Name;

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
