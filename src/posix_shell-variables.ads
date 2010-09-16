with GNAT.OS_Lib; use GNAT.OS_Lib;
with Posix_Shell.Annotated_Strings; use Posix_Shell.Annotated_Strings;

package Posix_Shell.Variables is

   Variable_Name_Error : exception;
   --  An exception raised when an invalid variable name is used.
   --  Invalid in this case is meant from a syntactic point of view;
   --  whether the variable exists or not is irrelevant in this case.

   function Is_Valid_Variable_Name (Name : String) return Boolean;
   --  Return  True if Name is, from a syntactic point of view, a valid
   --  variable name, False otherwise.

   --  function Get_Var_Value (Name : String) return String_Access;
   --  If Name is a defined variable name, then return a pointer
   --  to its current value.  Return null otherwise.
   --
   --  The pointer returned should be deallocated after use.

   function Get_Var_Value (Name : String) return String;
   --  Return the value of a variable. The empty string is returned
   --  when the variable is not defined.

   function Get_Var_Value
     (Name            : String;
      Context         : Annotation;
      Check_Existence : Boolean    := True)
      return Annotated_String;

   function Is_Var_Set (Name : String) return Boolean;

   procedure Import_Environment;

   procedure Export_Environment;

   function Get_Environment return String_List;

   procedure Set_Var_Value
     (Name         : String;
      Value        : String;
      Export       : Boolean := False;
      Is_Env_Value : Boolean := False);
   --  Set a var value

   procedure Unset_Var (Name : String);
   --  Unset variable

   procedure Export_Var (Name : String);
   --  Export a variable

   procedure Export_Var (Name : String; Value : String);
   --  Export variable

   procedure Enter_Scope;
   --  Enter a new scope

   procedure Leave_Scope (Keep_Pos_Params : Boolean := False);
   --  restore previous env values

   procedure Save_Last_Exit_Status (Exit_Status : Integer);
   --  Save the exit status of the last command we have run.

   function Get_Last_Exit_Status return Integer;
   --  Return the exit code returned by the last command we have run.

   type Pos_Params_State is private;

   procedure Set_Positional_Parameters (Args : String_List);

   function Set_Positional_Parameters
     (Args : String_List)
      return Pos_Params_State;

   procedure Restore_Positional_Parameters
     (State : Pos_Params_State);

   function Shift_Positional_Parameters (N : Natural := 1) return Boolean;
   --  Shift the positional parameters by N elements. In other words,
   --  ${N+1} becomes $1, ${N+2} becomes $2, etc...  If N is greater
   --  than the number of positional parameters, then the shift is
   --  not performed, and Success is set to False.
private

   type Pos_Params_State is record
      Table : String_List_Access := null;
      Shift : Integer := 0;
      Scope : Integer := 0;
   end record;

end Posix_Shell.Variables;
