------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Variables                          --
--                                                                          --
--                                 S p e c                                  --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Posix_Shell.Tree; use Posix_Shell.Tree;
with OS.FS;

package Posix_Shell.Variables is

   type Shell_State is private;
   type Shell_State_Access is access all Shell_State;
   --  Due to the approach taken in the implementation to use threads instead
   --  of processes (no fork on Windows), the shell state contains all the
   --  state needed by a shell "process". This means:
   --    * current directory
   --    * file descriptors (include stdout, stderr, ...)
   --    * environment variables,...
   --
   --  Inside GSH you should not rely on usual process APIs to retrieve these
   --  elements but use the API in this unit and its children

   ---------------------------
   -- Environment Variables --
   ---------------------------

   Variable_Name_Error : exception;
   --  An exception raised when an invalid variable name is used.
   --  Invalid in this case is meant from a syntactic point of view;
   --  whether the variable exists or not is irrelevant in this case.

   function Get_Var_Value (State : Shell_State; Name : String) return String;
   --  Return the value of a variable. The empty string is returned
   --  when the variable is not defined.

   function Get_Var_Value
     (State           : Shell_State;
      Name            : String;
      Is_Splitable    : Boolean := True;
      Check_Existence : Boolean := True)
      return Annotated_String;

   function Is_Var_Set (State : Shell_State; Name : String) return Boolean;

   procedure Import_Environment (State : in out Shell_State);

   function Get_Environment (State : Shell_State) return String_List;

   procedure Set_Var_Value
     (State        : in out Shell_State;
      Name         : String;
      Value        : String;
      Export       : Boolean := False;
      Is_Env_Value : Boolean := False);
   --  Set a var value

   procedure Unset_Var (State : in out Shell_State; Name : String);
   --  Unset variable

   procedure Export_Var (State : in out Shell_State; Name : String);
   --  Export a variable

   procedure Export_Var
     (State : in out Shell_State; Name : String; Value : String);
   --  Export variable

   -----------------------------
   -- Paths related functions --
   -----------------------------

   function Get_Current_Dir
     (State : Shell_State; Strip_Drive : Boolean := False) return String;

   procedure Set_Current_Dir (State : in out Shell_State; Dir : String);

   function Resolve_Path
     (State : Shell_State; Path : String) return String;

   function Enter_Scope (Previous : Shell_State) return Shell_State;
   --  Given the current state create a new scope and return its state

   procedure Leave_Scope
     (Current  : in out Shell_State;
      Previous : in out Shell_State);
   --  restore previous env values

   procedure Save_Last_Exit_Status
     (State : in out Shell_State; Exit_Status : Integer);
   --  Save the exit status of the last command we have run.

   function Get_Last_Exit_Status (State : Shell_State) return Integer;
   --  Return the exit code returned by the last command we have run.

   ---------------------------
   -- Positional Parameters --
   ---------------------------

   type Pos_Params_State is private;

   procedure Set_Positional_Parameters
     (State         : in out Shell_State;
      Args          : String_List;
      Free_Previous : Boolean := True);

   function Get_Positional_Parameters
     (State : Shell_State) return Pos_Params_State;

   procedure Restore_Positional_Parameters
     (State : in out Shell_State; Pos_Params : Pos_Params_State);

   procedure Shift_Positional_Parameters
     (State : in out Shell_State; N : Natural; Success : out Boolean);
   --  Shift the positional parameters by N elements. In other words,
   --  ${N+1} becomes $1, ${N+2} becomes $2, etc...  If N is greater
   --  than the number of positional parameters, then the shift is
   --  not performed, and Success is set to False.

   procedure Deallocate (S : in out Shell_State_Access);

   function Get_Loop_Scope_Level (S : Shell_State) return Natural;

   procedure Set_Loop_Scope_Level (S : in out Shell_State; N : Natural);

   procedure Set_Script_Name (S : in out Shell_State; Value : String);

   procedure Set_Xtrace (S : in out Shell_State; Value : Boolean);

   procedure Set_File_Expansion (S : in out Shell_State; Value : Boolean);

   function Is_Xtrace_Enabled (S : Shell_State) return Boolean;

   function Is_File_Expansion_Enabled (S : Shell_State) return Boolean;

   procedure Set_Trap_Action
     (S : in out Shell_State;
      Action : String_Access;
      Signal_Number : Integer);

   function Get_Trap_Action
     (S : Shell_State; Signal_Number : Integer)
      return String_Access;

   function Is_Function (S : Shell_State; Name : String) return Boolean;
   function Get_Function (S : Shell_State; Name : String) return Shell_Tree;
   procedure Register_Function (S    : in out Shell_State;
                                Name : String;
                                Tree : Shell_Tree);

   type Shell_Descriptors is private;

private

   type Pos_Params_State is record
      Table : String_List_Access := null;
      Shift : Integer := 0;
      Scope : Integer := 0;
   end record;

   type Var_Value is record
      Val         : String_Access;
      Env_Val     : String_Access;
      Is_Exported : Boolean := False;
      Scope_Owner : Natural := 0;
   end record;

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String,
        Var_Value,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use String_Maps;

   type Function_Map_Element is record
      Code  : Shell_Tree;
      Owner : Boolean;
   end record;

   package Function_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Function_Map_Element,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Function_Maps;

   --  A table that maps function names to their associated Node.
   type Shell_Descriptor is record
      Fd              : OS.FS.File_Descriptor;
      Filename        : String_Access;
      Delete_On_Close : Boolean;
      Can_Be_Closed   : Boolean;
   end record;
   --  State of Stdin, Stdout or Stderr (file descriptor and filename
   --  if relevant).

   type Shell_Descriptors is array (-2 .. 13) of Shell_Descriptor;
   --  State of Stdin, Stdout and Stderr.
   --  ??? brobecker/2007-04-23:
   --  ???    I think that 0 .. 2 are stdin, stdout and stderr,
   --  ???    and 3 .. 4 are pipe-in and pipe-out.

   --  procedure Push_Redirections
   --  (S : in out Shell_State; R : Redirection_Op_Stack);
   --  Set a new redirection context.

   type Trap_Action_List is array (0 .. 15) of String_Access;

   type Shell_State is record
      Var_Table              : String_Maps.Map;
      Fun_Table              : Function_Maps.Map;
      Last_Exit_Status       : Integer := 0;
      Pos_Params             : Pos_Params_State;
      Scope_Level            : Natural := 1;
      Is_Env_Valid           : Boolean := False;
      Redirections           : Shell_Descriptors;
      Current_Dir            : String_Access := null;
      Script_Name            : String_Access := null;
      Trap_Actions           : Trap_Action_List := (others => null);
      Loop_Scope_Level       : Natural := 0;
      XTrace_Enabled         : Boolean := False;
      File_Expansion_Enabled : Boolean := True;
   end record;

end Posix_Shell.Variables;
