with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Posix_Shell.Opts; use Posix_Shell.Opts;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Dyn_String_Lists; use Dyn_String_Lists;

pragma Elaborate_All (Dyn_String_Lists);

package body Posix_Shell.Variables is

   Max_Scope : constant Integer := 64;
   Top_Level_Scope : constant Integer := 1;

   Is_Env_Valid : Boolean := False;

   subtype Scope_Level is Integer range Top_Level_Scope .. Max_Scope;

   Scope : Scope_Level := 1;
   --  Global variable containing the current scope level

   type Var_Value is record
      Val         : String_Access;
      Env_Val     : String_Access;
      Is_Exported : Boolean := False;
      Scope_Owner : Scope_Level := 1;
   end record;

   --  Null_Var_Value : constant Var_Value := (null, False, 1);

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String,
        Var_Value,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use String_Maps;

   type Scope_Context is record
      Var_Table             : Map;
      Last_Exit_Status      : Integer := 0;
      Pos_Params            : Pos_Params_State;
   end record;

   type Contexts is array (Top_Level_Scope .. Max_Scope) of Scope_Context;

   Global_Context : Contexts;

   function Portable_Getpid return Interfaces.C.long;
   pragma Import (C, Portable_Getpid, "getpid");

   function Is_Positional_Parameter (Name : String) return Boolean;

   procedure Check_Variable_Name (Name : String);
   --  Raise Variable_Name_Error if the given variable name is not
   --  syntactically correct.

   -------------------------
   -- Check_Variable_Name --
   -------------------------

   procedure Check_Variable_Name (Name : String) is
   begin
      if not Is_Valid_Variable_Name (Name) then
         raise Variable_Name_Error;
      end if;
   end Check_Variable_Name;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope is
      procedure Enter_Scope_Aux (Position : Cursor);

      ---------------------
      -- Enter_Scope_Aux --
      ---------------------

      procedure Enter_Scope_Aux (Position : Cursor) is
         V : constant Var_Value := Element (Position);
         K : constant String := Key (Position);
      begin
         Insert (Global_Context (Scope).Var_Table, K, V);
      end Enter_Scope_Aux;
   begin
      Scope := Scope + 1;
      --  Create a new var table based on the previous context
      Reserve_Capacity (Global_Context (Scope).Var_Table, 256);
      Iterate (Global_Context (Scope - 1).Var_Table,
               Enter_Scope_Aux'Unrestricted_Access);

      --  Copy also the positional parameters status
      Global_Context (Scope).Pos_Params :=
        Global_Context (Scope - 1).Pos_Params;
   end Enter_Scope;

   ------------------------
   -- Export_Environment --
   ------------------------

   procedure Export_Environment is

      procedure Export_Aux (Position : Cursor);

      procedure Export_Aux (Position : Cursor) is
         V : constant Var_Value := Element (Position);
         K : constant String := Key (Position);
         --  Upper_Name : constant String := Translate (K, Upper_Case_Map);
      begin
         if V.Is_Exported then
            Set (K, V.Val.all);
         elsif V.Env_Val /= null then
            Set (K, V.Env_Val.all);
         end if;
      end Export_Aux;

   begin
      --  Reset environment
      if not Is_Env_Valid then
         Clear;
         Iterate (Global_Context (Scope).Var_Table,
                  Export_Aux'Unrestricted_Access);
         Is_Env_Valid := True;
      end if;
   end Export_Environment;

   ---------------------
   -- Get_Environment --
   ---------------------

   Get_Env_Cache : Dyn_String_List;

   function Get_Environment return String_List is

      Result : Dyn_String_List;
      procedure Export_Aux (Position : Cursor);

      procedure Export_Aux (Position : Cursor) is
         V : constant Var_Value := Element (Position);
         K : constant String := Key (Position);
         --  Upper_Name : constant String := Translate (K, Upper_Case_Map);
      begin
         if V.Is_Exported then
            Append (Result, new String'(K & "=" & V.Val.all & ASCII.NUL));
         elsif V.Env_Val /= null then
            Append (Result, new String'(K & "=" & V.Env_Val.all & ASCII.NUL));
         end if;
      end Export_Aux;

   begin
      if Is_Env_Valid then
         return Content (Get_Env_Cache);
      else
         --  Reset environment
         Iterate (Global_Context (Scope).Var_Table,
                  Export_Aux'Unrestricted_Access);
         Get_Env_Cache := Result;
         Is_Env_Valid := True;
         return Content (Get_Env_Cache);
      end if;
   end Get_Environment;

   ----------------
   -- Export_Var --
   ----------------

   procedure Export_Var (Name : String) is
      V : constant String := Get_Var_Value (Name);
   begin
      Set_Var_Value (Name, V, True);
   end Export_Var;

   ----------------
   -- Export_Var --
   ----------------

   procedure Export_Var (Name : String; Value : String) is
   begin
      Set_Var_Value (Name, Value, True);
   end Export_Var;

   --------------------------
   -- Get_Last_Exit_Status --
   --------------------------

   function Get_Last_Exit_Status return Integer is
   begin
      return Global_Context (Scope).Last_Exit_Status;
   end Get_Last_Exit_Status;

   -------------------
   -- Get_Var_Value --
   -------------------

   function Get_Var_Value
     (Name            : String;
      Context         : Annotation;
      Check_Existence : Boolean    := True)
      return Annotated_String
   is
      function Path_Transform
        (S    : String;
         Name : String)
         return String;

      Upper_Name : constant String := Translate (Name, Upper_Case_Map);

      function Path_Transform (S : String; Name : String) return String is
      begin
         if (Name = "PATH" or else Name = "HOME") and then
           Directory_Separator = '\'
         then
            declare
               Result : String (1 .. S'Length);
               Result_Last : Integer := 0;
               Is_First : Boolean := True;
               Index : Natural := S'First;
            begin
               loop
                  if Is_First and then Index + 1 <= S'Last and then
                    S (Index + 1) = ':'
                  then
                     Index := Index + 1;
                     Is_First := False;
                  elsif S (Index) = ';' then
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := ':';
                     Is_First := True;
                  elsif S (Index) = '\' then
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '/';
                  else
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := S (Index);
                  end if;
                  Index := Index + 1;
                  exit when Index > S'Last;
               end loop;
               return Result (1 .. Result_Last);
            end;

         else
            return S;
         end if;
      end Path_Transform;

      Result : Annotated_String;
   begin
      --  If necessary check for variable existence
      if Check_Existence then
         if not Is_Var_Set (Name) then
            return Result;
         end if;
      end if;

      --  Starting from here we assume the variable exist

      if Is_Positional_Parameter (Name) then
         declare
            Index : constant Integer := Integer'Value (Name);
            Shift : constant Integer :=
              Global_Context (Scope).Pos_Params.Shift;
         begin
            Append (Result,
                    Global_Context (Scope).Pos_Params.Table
                    (Index + Shift).all,
                    Context);
            return Result;
         end;
      end if;

      if Name'Length = 1 then
         --  Expansion of Special parameters (XCU section 2.5.2)
         case Name (Name'First) is
            when '?' =>
               Append (Result, To_String (Get_Last_Exit_Status), Context);
               return Result;
            when '$' =>
               Append (Result,
                       Trim (long'Image (Portable_Getpid), Ada.Strings.Both),
                       Context);
               return Result;
               --  We didn't use To_String to perform the translation,
               --  because the PID could be larger than Integer'Last on
               --  certain platforms.
            when '@' =>
               --  As stated in 2.5.2, field splitting should always be
               --  performed when expanding '@' even if IFS is set to '' or if
               --  we are in a quoted context
               declare
                  PP : constant Pos_Params_State :=
                    Global_Context (Scope).Pos_Params;
               begin
                  for J in PP.Table'First + PP.Shift .. PP.Table'Last loop
                     Append (Result, PP.Table (J).all, Context);

                     --  Add a 'forced' field separator which is not influenced
                     --  by IFS variable value
                     if J < PP.Table'Last then
                           Append (Result, ' ', FIELD_SEP);
                     end if;
                  end loop;
                  return Result;
               end;
            when '*' =>
               declare
                  Sep      : Character := ' ';
                  --  Separator used when expanding $*. The default value is a
                  --  space and it is used when IFS is not set

                  Have_Sep : Boolean := True;
                  --  Set to true when a separator should be added between each
                  --  positional parameter expansion.

                  PP : constant Pos_Params_State :=
                    Global_Context (Scope).Pos_Params;
               begin
                  --  First compute the field separator if we are in a quoted
                  --  context.
                  if Is_Var_Set ("IFS") then
                     declare
                        IFS : constant String := Get_Var_Value ("IFS");
                     begin
                        if IFS'Length = 0 then
                           --  IFS is set but has a null value. In that case we
                           --  just concatenate the positional parameters as
                           --  stated in XCU 2.5.2
                           Have_Sep := False;
                        else
                           Sep := IFS (IFS'First);
                        end if;
                     end;
                  end if;

                  for J in PP.Table'First + PP.Shift .. PP.Table'Last loop
                     Append (Result, PP.Table (J).all, Context);
                     if J < PP.Table'Last then
                        if Context = NO_ANNOTATION and then not Have_Sep then
                           Append (Result, ' ', FIELD_SEP);
                        elsif Have_Sep then
                           Append (Result, Sep, Context);
                        end if;
                     end if;
                  end loop;

                  return Result;
               end;
            when '!' => return To_Annotated_String ("", Context);
            when '-' => return To_Annotated_String ("", Context);
            when '#' =>
               declare
                  Real_Size : constant Integer :=
                    Global_Context (Scope).Pos_Params.Table'Length;
                  Shift : constant Integer :=
                    Global_Context (Scope).Pos_Params.Shift;
               begin
                  Append (Result, To_String (Real_Size - Shift), Context);
                  return Result;
               end;
            when '0' =>
               Append (Result, Script_Name, Context);
               return Result;
            when others => null;
         end case;
      end if;

      if Contains (Global_Context (Scope).Var_Table, Name) then
         Append
           (Result,
            Path_Transform (Element
              (Global_Context (Scope).Var_Table, Name).Val.all, Name),
            Context);
      elsif Contains (Global_Context (Scope).Var_Table, Upper_Name) then
         declare
            Tmp : constant Var_Value := Element
              (Global_Context (Scope).Var_Table, Upper_Name);
         begin
            if Tmp.Env_Val /= null then
               Append
                 (Result,
                  Path_Transform (Tmp.Env_Val.all, Upper_Name),
                  Context);
            end if;
         end;
      end if;
      return Result;
   end Get_Var_Value;

   -------------------
   -- Get_Var_Value --
   -------------------

   function Get_Var_Value (Name : String) return String is
      Value : constant Annotated_String := Get_Var_Value (Name, NO_ANNOTATION);
   begin
      return Str (Value);
   end Get_Var_Value;

   ------------------------
   -- Import_Environment --
   ------------------------

   procedure Import_Environment is
      procedure Import_Env_Aux (Name, Value : String);

      procedure Import_Env_Aux (Name, Value : String) is
         Upper_Name : constant String := Translate (Name, Upper_Case_Map);
      begin
         if Is_Valid_Variable_Name (Upper_Name) then
            Set_Var_Value (Upper_Name, Value, False, True);
         end if;
      end Import_Env_Aux;

   begin
      Iterate (Import_Env_Aux'Unrestricted_Access);
      declare
         Tmp : constant String_List := Get_Environment;
         pragma Unreferenced (Tmp);
      begin
         Is_Env_Valid := True;
      end;
   end Import_Environment;

   -----------------------------
   -- Is_Positional_Parameter --
   -----------------------------

   function Is_Positional_Parameter (Name : String) return Boolean is
   begin
      for J in Name'Range loop
         if Name (J) not in '0' .. '9' then
            return False;
         end if;
      end loop;

      --  "$0" is not a positional parameter, it is a special parameter.
      if Name = "0" then
         return False;
      end if;

      return True;
   end Is_Positional_Parameter;

   ----------------------------
   -- Is_Valid_Variable_Name --
   ----------------------------

   function Is_Valid_Variable_Name (Name : String) return Boolean is
   begin
      --  For the definition of valid names, see: the Base Definitions
      --  volume of IEEE Std 1003.1-2001, Section 3.230, Name.
      --  ??? brobecker/2007-04-28: Double-check the implementation below
      --  ??? against the document above. I haven't been able to do so
      --  ??? at the time I implemented this, because I don't have access
      --  ??? to internet right now.

      --  Name must be at least one character long

      if Name'Length = 0 then
         return False;
      end if;

      --  The first character must be alphabetic

      if not Is_Letter (Name (Name'First)) and Name (Name'First) /= '_' then
         return False;
      end if;

      --  The remaining characters must be either alpha-numeric
      --  or an underscore.

      for J in Name'First + 1 .. Name'Last loop
         if not Is_Alphanumeric (Name (J)) and then Name (J) /= '_' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Variable_Name;

   ----------------
   -- Is_Var_Set --
   ----------------

   function Is_Var_Set (Name : String) return Boolean is
      Upper_Name : constant String := Translate (Name, Upper_Case_Map);
   begin
      --  Is this a positional parameter
      if Is_Positional_Parameter (Name) then
         declare
            Index : constant Integer := Integer'Value (Name);
            Shift : constant Integer :=
              Global_Context (Scope).Pos_Params.Shift;
         begin
            if Index + Shift <=
              Global_Context (Scope).Pos_Params.Table'Last
            then
               return True;
            else
               return False;
            end if;
         end;
      end if;

      if Name'Length = 1 then
         case Name (Name'First) is
            when '?' | '$' | '@' | '*' | '-' | '!' | '#' | '0' => return True;
            when others => null;
         end case;
      end if;

      Check_Variable_Name (Name);

      --  Try in the list of local variables...
      if Contains (Global_Context (Scope).Var_Table, Name) then
         return True;
      elsif Contains (Global_Context (Scope).Var_Table, Upper_Name) and then
        Element (Global_Context (Scope).Var_Table, Upper_Name).Env_Val /= null
      then
         return True;
      end if;

      --  Variable does not exist.
      return False;
   end Is_Var_Set;

   -----------------
   -- Leave_Scope --
   -----------------

   procedure Leave_Scope (Keep_Pos_Params : Boolean := False) is
      procedure Leave_Scope_Aux (Position : Cursor);

      ---------------------
      -- Leave_Scope_Aux --
      ---------------------

      procedure Leave_Scope_Aux (Position : Cursor) is
         V : Var_Value := Element (Position);
      begin
         if V.Scope_Owner = Scope + 1 and then V.Val /= null then
            Free (V.Val);
         end if;

      end Leave_Scope_Aux;
   begin

      Scope := Scope - 1;

      --  Clear variable table
      Iterate (Global_Context (Scope + 1).Var_Table,
               Leave_Scope_Aux'Unrestricted_Access);
      Clear (Global_Context (Scope + 1).Var_Table);

      --  Propagate exit status
      Global_Context (Scope).Last_Exit_Status :=
        Global_Context (Scope + 1).Last_Exit_Status;

      --  Clear positional parameters
      if Global_Context (Scope + 1).Pos_Params.Scope = Scope + 1 then
         if Keep_Pos_Params then
            if Global_Context (Scope).Pos_Params.Scope = Scope then
               for J in Global_Context (Scope).Pos_Params.Table'Range loop
                  Free (Global_Context (Scope).Pos_Params.Table (J));
               end loop;
            end if;
            Global_Context (Scope).Pos_Params :=
              Global_Context (Scope + 1).Pos_Params;
         else
            for J in Global_Context (Scope + 1).Pos_Params.Table'Range loop
               Free (Global_Context (Scope + 1).Pos_Params.Table (J));
            end loop;
         end if;
      end if;

   end Leave_Scope;

   -----------------------------------
   -- Restore_Positional_Parameters --
   -----------------------------------

   procedure Restore_Positional_Parameters (State : Pos_Params_State) is
   begin
      --  Free if necessary previous parameters
      if Global_Context (Scope).Pos_Params.Scope = Scope then
         for J in Global_Context (Scope).Pos_Params.Table'Range loop
            Free (Global_Context (Scope).Pos_Params.Table (J));
         end loop;
      end if;

      Global_Context (Scope).Pos_Params := State;
   end Restore_Positional_Parameters;

   ---------------------------
   -- Save_Last_Exit_Status --
   ---------------------------

   procedure Save_Last_Exit_Status (Exit_Status : Integer) is
   begin
      Global_Context (Scope).Last_Exit_Status :=  Exit_Status;
   end Save_Last_Exit_Status;

   function Set_Positional_Parameters
     (Args : String_List)
      return Pos_Params_State
   is
      Args_Copy : constant String_List_Access
        := new String_List (1 .. Args'Length);
      Result : constant Pos_Params_State := Global_Context (Scope).Pos_Params;
   begin
      for J in 1 .. Args'Length loop
         --  We take the pain of readjusting the index of this array
         --  so that the first index is 1.  That way, later one, when
         --  we need to access say $3, we know that its value is at
         --  index 3 of our array.
         Args_Copy (J) := new String'(Args (Args'First + J - 1).all);
      end loop;

      --  Set the new parameters
      Global_Context (Scope).Pos_Params.Table := Args_Copy;
      Global_Context (Scope).Pos_Params.Scope := Scope;
      Global_Context (Scope).Pos_Params.Shift := 0;
      return Result;
   end Set_Positional_Parameters;

   -------------------------------
   -- Set_Positional_Parameters --
   -------------------------------

   procedure Set_Positional_Parameters (Args : String_List) is
      Args_Copy : constant String_List_Access
        := new String_List (1 .. Args'Length);
   begin
      for J in 1 .. Args'Length loop
         --  We take the pain of readjusting the index of this array
         --  so that the first index is 1.  That way, later one, when
         --  we need to access say $3, we know that its value is at
         --  index 3 of our array.
         Args_Copy (J) := new String'(Args (Args'First + J - 1).all);
      end loop;

      --  Free if necessary previous parameters
      if Global_Context (Scope).Pos_Params.Scope = Scope then
         for J in Global_Context (Scope).Pos_Params.Table'Range loop
            Free (Global_Context (Scope).Pos_Params.Table (J));
         end loop;
      end if;

      --  Set the new parameters
      Global_Context (Scope).Pos_Params := (Args_Copy, 0, Scope);
   end Set_Positional_Parameters;

   -------------------
   -- Set_Var_Value --
   -------------------

   procedure Set_Var_Value
     (Name   : String;
      Value  : String;
      Export : Boolean := False;
      Is_Env_Value : Boolean := False)
   is
      V : Var_Value := (null, null, False, Scope);
      Is_New_Var : constant Boolean := not Contains
        (Global_Context (Scope).Var_Table, Name);

      function Path_Transform (S : String; Name : String) return String;

      function Path_Transform (S : String; Name : String) return String is
      begin
         if (Name = "PATH" or else Name = "HOME") and then
           Directory_Separator = '\'
         then
            declare
               Result : String (1 .. S'Length * 3);
               Result_Last : Integer := 0;
               Is_First : Boolean := True;
               Index : Natural := S'First;
            begin
               loop
                  if S (Index) = ':' then
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := ';';
                     Is_First := True;
                  elsif S (Index) = '/' then
                     if Is_First then
                        Result_Last := Result_Last + 2;
                        Result (Result_Last - 1) := 'C';
                        Result (Result_Last) := ':';
                     end if;
                     Is_First := False;
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '\';
                  else
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := S (Index);
                  end if;
                  Index := Index + 1;
                  exit when Index > S'Last;
               end loop;
               return Result (1 .. Result_Last);
            end;

         else
            return S;
         end if;
      end Path_Transform;

      Effective_Value : constant String := Path_Transform (Value, Name);

   begin
      Check_Variable_Name (Name);

      if not Is_New_Var then
         --  This is not a new variable so retrieve its value
         V := Element (Global_Context (Scope).Var_Table, Name);

         if V.Scope_Owner = Scope and V.Val /= null then
            --  The variable has been declared in the current context, so we
            --  can safely free the current value
            Free (V.Val);
         end if;

         if Export then
            V.Is_Exported := True;
         end if;

         if Is_Env_Value then
            --  Here we are in the context of the import_environment function
            --  the value passed to Set_Var_Value has not been transformed
            V.Val := new String'(Value);
         else
            --  This is the usual context so store the real Windows value by
            --  doing the Unix2Dos path transformation.
            V.Val := new String'(Effective_Value);
         end if;

         V.Scope_Owner := Scope;

         --  Update the variable table
         Replace (Global_Context (Scope).Var_Table, Name, V);

         --  If the variable is exported, the current env is no more valid.
         if V.Is_Exported then
            Is_Env_Valid := False;
         end if;
      else
         --  The variable is new
         if Is_Env_Value then
            --  We are in the context of Import_Environment.
            Include (Global_Context (Scope).Var_Table, Name,
              (new String'(Value),
               new String'(Value), Export, Scope));
         else
            Include (Global_Context (Scope).Var_Table, Name,
                     (new String'(Effective_Value), null, Export, Scope));
         end if;

         --  Invalidate current env
         if Export then
            Is_Env_Valid := False;
         end if;
      end if;

   end Set_Var_Value;

   ---------------------------------
   -- Shift_Positional_Parameters --
   ---------------------------------

   function Shift_Positional_Parameters (N : Natural := 1) return Boolean is
      Shift : constant Integer := Global_Context (Scope).Pos_Params.Shift;
      Pos_Params_Size : constant Integer :=
        Global_Context (Scope).Pos_Params.Table.all'Length;
   begin
      if Shift + N > Pos_Params_Size then
         --  Shift is too large, report an error.
         return False;
      end if;

      Global_Context (Scope).Pos_Params.Shift := Shift + N;
      return True;
   end Shift_Positional_Parameters;

   ---------------
   -- Unset_Var --
   ---------------

   procedure Unset_Var (Name : String) is
   begin
      if Contains (Global_Context (Scope).Var_Table, Name) then
         declare
            V : Var_Value := Element
              (Global_Context (Scope).Var_Table, Name);
         begin
            if V.Scope_Owner = Scope and V.Val /= null then
               --  The variable has been declared in the current context, so we
               --  can safely free the current value
               Free (V.Val);
            end if;
            Delete (Global_Context (Scope).Var_Table, Name);
         end;
      end if;
   end Unset_Var;

end Posix_Shell.Variables;
