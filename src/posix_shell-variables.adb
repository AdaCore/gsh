------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                           Posix_Shell.Variables                          --
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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Posix_Shell.String_Utils; use Posix_Shell.String_Utils;

package body Posix_Shell.Variables is

   --  Global variable containing the current scope

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

   procedure Deallocate (S : in out Shell_State_Access) is
      procedure Internal_Free is
        new Ada.Unchecked_Deallocation
          (Shell_State,
           Shell_State_Access);
   begin
      Internal_Free (S);
   end Deallocate;

   -----------------
   -- Enter_Scope --
   -----------------

   function Enter_Scope (Previous : Shell_State) return Shell_State is

      Result : Shell_State;

      procedure Enter_Scope_Aux (Position : Cursor);

      ---------------------
      -- Enter_Scope_Aux --
      ---------------------

      procedure Enter_Scope_Aux (Position : Cursor) is
         V : constant Var_Value := Element (Position);
         K : constant String := Key (Position);
      begin
         Insert (Result.Var_Table, K, V);
      end Enter_Scope_Aux;
   begin
      Result.Scope_Level := Previous.Scope_Level + 1;
      --  Create a new var table based on the previous context
      Reserve_Capacity (Result.Var_Table, 256);
      Iterate (Previous.Var_Table,
               Enter_Scope_Aux'Unrestricted_Access);

      --  Copy also the positional parameters status
      Result.Pos_Params := Previous.Pos_Params;
      Result.Redirections := Previous.Redirections;

      --  Copy the current directory information
      Result.Current_Dir := new String'(Previous.Current_Dir.all);

      --  Reset to 0 for/until/while nested level
      Result.Loop_Scope_Level := 0;

      Result.XTrace_Enabled := Previous.XTrace_Enabled;
      Result.File_Expansion_Enabled := Previous.File_Expansion_Enabled;

      Result.Script_Name := Previous.Script_Name;

      return Result;
   end Enter_Scope;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment (State : Shell_State) return String_List is

      Result      : String_List (1 .. Integer (Length (State.Var_Table)));
      Result_Last : Natural := 0;

      procedure Export_Aux (Position : Cursor);

      procedure Export_Aux (Position : Cursor) is
         V : constant Var_Value := Element (Position);
         K : constant String := Key (Position);
         --  Upper_Name : constant String := Translate (K, Upper_Case_Map);
      begin
         if V.Is_Exported then
            Result_Last := Result_Last + 1;
            Result (Result_Last) :=
              new String'(K & "=" & V.Val.all & ASCII.NUL);
         elsif V.Env_Val /= null then
            Result_Last := Result_Last + 1;
            Result (Result_Last) :=
              new String'(K & "=" & V.Env_Val.all & ASCII.NUL);
         end if;
      end Export_Aux;

   begin
      --  Reset environment
      Iterate (State.Var_Table,
               Export_Aux'Unrestricted_Access);

      return Result (1 .. Result_Last);
   end Get_Environment;

   ----------------
   -- Export_Var --
   ----------------

   procedure Export_Var (State : in out Shell_State; Name : String) is
      V : constant String := Get_Var_Value (State, Name);
   begin
      Set_Var_Value (State, Name, V, True);
   end Export_Var;

   ----------------
   -- Export_Var --
   ----------------

   procedure Export_Var
     (State : in out Shell_State; Name : String; Value : String)
   is
   begin
      Set_Var_Value (State, Name, Value, True);
   end Export_Var;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (State : Shell_State; Strip_Drive : Boolean := False) return String
   is
      Cur : constant String :=  State.Current_Dir.all;
   begin
      if Strip_Drive and then Cur (Cur'First + 1) = ':' then
         if Cur'Length = 2 then
            return "/";
         else
            return Cur (Cur'First + 2 .. Cur'Last);
         end if;
      else
         return Cur;
      end if;
   end Get_Current_Dir;

   --------------------------
   -- Get_Last_Exit_Status --
   --------------------------

   function Get_Last_Exit_Status (State : Shell_State) return Integer is
   begin
      return State.Last_Exit_Status;
   end Get_Last_Exit_Status;

   -------------------
   -- Get_Var_Value --
   -------------------

   function Get_Var_Value
     (State           : Shell_State;
      Name            : String;
      Is_Splitable    : Boolean := True;
      Check_Existence : Boolean := True)
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
               Result      : String (1 .. S'Length);
               Result_Last : Integer := 0;
               Is_First    : Boolean := True;
               Index       : Natural := S'First;
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
         if not Is_Var_Set (State, Name) then
            return Result;
         end if;
      end if;

      --  Starting from here we assume the variable exist

      if Is_Positional_Parameter (Name) then
         declare
            Index : constant Integer := Integer'Value (Name);
            Shift : constant Integer := State.Pos_Params.Shift;
         begin
            Append (Result,
                    State.Pos_Params.Table
                    (Index + Shift).all);
            return Result;
         end;
      end if;

      if Name'Length = 1 then
         --  Expansion of Special parameters (XCU section 2.5.2)
         case Name (Name'First) is
            when '?' =>
               Append
                 (Result,
                  To_String (Get_Last_Exit_Status (State)));
               return Result;
            when '$' =>
               Append (Result,
                       Trim (long'Image (Portable_Getpid), Ada.Strings.Both));
               return Result;
               --  We didn't use To_String to perform the translation,
               --  because the PID could be larger than Integer'Last on
               --  certain platforms.
            when '@' =>
               --  As stated in 2.5.2, field splitting should always be
               --  performed when expanding '@' even if IFS is set to '' or if
               --  we are in a quoted context
               declare
                  PP : constant Pos_Params_State := State.Pos_Params;
                  Has_Parameter : Boolean := False;
               begin
                  for J in PP.Table'First + PP.Shift .. PP.Table'Last loop
                     Has_Parameter := True;
                     Append (Result, PP.Table (J).all);

                     --  Add a 'forced' field separator which is not influenced
                     --  by IFS variable value
                     if J < PP.Table'Last then
                        Append (Result, FIELD_SEP);
                     end if;
                  end loop;

                  if not Has_Parameter and not Is_Splitable then
                     --  There is no parameter so we will return a null string.
                     --  But for @ variable, even if we are inside a quote
                     --  construct, we should not at the end return '' but
                     --  nothing. Example:
                     --
                     --  for d in "$EMPTY_VAR"; do echo "hello"; done
                     --  for d in "$@"; do echo "hello2"; done
                     --
                     --  should output "hello" :-)
                     Append (Result, QUOTED_NULL_STRING);
                  end if;
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

                  PP : constant Pos_Params_State := State.Pos_Params;
               begin
                  --  First compute the field separator if we are in a quoted
                  --  context.
                  if Is_Var_Set (State, "IFS") then
                     declare
                        IFS : constant String := Get_Var_Value (State, "IFS");
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
                     Append (Result, PP.Table (J).all);
                     if J < PP.Table'Last then
                        if Is_Splitable and then not Have_Sep then
                           Append (Result, FIELD_SEP);
                        elsif Have_Sep then
                           Append (Result, Sep);
                        end if;
                     end if;
                  end loop;

                  return Result;
               end;
            when '!' => return Result;
            when '-' => return Result;
            when '#' =>
               declare
                  Real_Size : constant Integer :=
                    State.Pos_Params.Table'Length;
                  Shift : constant Integer :=
                    State.Pos_Params.Shift;
               begin
                  Append (Result, To_String (Real_Size - Shift));
                  return Result;
               end;
            when '0' =>
               Append (Result, State.Script_Name.all);
               return Result;
            when others => null;
         end case;
      end if;

      if Contains (State.Var_Table, Name) then
         Append
           (Result,
            Path_Transform (Element
              (State.Var_Table, Name).Val.all, Name));
      elsif Contains (State.Var_Table, Upper_Name) then
         declare
            Tmp : constant Var_Value := Element
              (State.Var_Table, Upper_Name);
         begin
            if Tmp.Env_Val /= null then
               Append
                 (Result,
                  Path_Transform (Tmp.Env_Val.all, Upper_Name));
            end if;
         end;
      end if;
      return Result;
   end Get_Var_Value;

   -------------------
   -- Get_Var_Value --
   -------------------

   function Get_Var_Value (State : Shell_State; Name : String) return String
   is
      Value : constant Annotated_String := Get_Var_Value (State, Name);
   begin
      return Str (Value);
   end Get_Var_Value;

   ------------------------
   -- Import_Environment --
   ------------------------

   procedure Import_Environment (State : in out Shell_State) is
      procedure Import_Env_Aux (Name, Value : String);

      procedure Import_Env_Aux (Name, Value : String) is
         Upper_Name : constant String := Translate (Name, Upper_Case_Map);
      begin
         if Is_Valid_Variable_Name (Upper_Name) then
            Set_Var_Value (State, Upper_Name, Value, False, True);
         end if;
      end Import_Env_Aux;

   begin
      --  First import environment to initialize our variables table
      Iterate (Import_Env_Aux'Unrestricted_Access);

      --  Initialize redirections to default values
      State.Redirections := (others => (Invalid_FD, null, False, False));
      State.Redirections (1) := (1, null, False, False);
      State.Redirections (2) := (2, null, False, False);
      State.Redirections (0) := (0, null, False, False);

      --  Initialize current directory
      --  Note that this is the only place where we get the current dir using
      --  information from the process. In any other places, user should use
      --  methods provided with the Shell_State. Indeed this implementation is
      --  using threads and the notion of current directory is bind to
      --  the process.
      declare
         Dir  : constant String := Format_Pathname (Get_Current_Dir, UNIX);
         Last : Integer := Dir'Last;
      begin
         if Dir (Dir'Last) = '/' then

         --  If this is the root directory, then we should not strip
         --  then ending directory separator.  Otherwise, we would end
         --  up creating an invalid directory name.
         --
         --  To determine independently of the type of filesystem whether
         --  the current directory is the root directory or not, we check
         --  whether we can find another directory separator before the
         --  end of Dir. If we find one, then Dir cannot be a root dir,
         --  in which case we should strip out the ending directory separator.
            for J in Dir'First .. Dir'Last - 1 loop
               if Dir (J) = '/' then
                  Last := Dir'Last - 1;
               end if;
            end loop;
         end if;

         State.Current_Dir := new String'(Dir (Dir'First .. Last));
      end;

      State.Loop_Scope_Level := 0;
   end Import_Environment;

   -------------------------------
   -- Is_File_Expansion_Enabled --
   -------------------------------

   function Is_File_Expansion_Enabled (S : Shell_State) return Boolean is
   begin
      return S.File_Expansion_Enabled;
   end Is_File_Expansion_Enabled;

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

   ----------------
   -- Is_Var_Set --
   ----------------

   function Is_Var_Set (State : Shell_State; Name : String) return Boolean is
      Upper_Name : constant String := Translate (Name, Upper_Case_Map);
   begin
      --  Is this a positional parameter
      if Is_Positional_Parameter (Name) then
         declare
            Index : constant Integer := Integer'Value (Name);
            Shift : constant Integer := State.Pos_Params.Shift;
         begin
            if Index + Shift <=
              State.Pos_Params.Table'Last
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
      if Contains (State.Var_Table, Name) then
         return True;
      elsif Contains (State.Var_Table, Upper_Name) and then
        Element (State.Var_Table, Upper_Name).Env_Val /= null
      then
         return True;
      end if;

      --  Variable does not exist.
      return False;
   end Is_Var_Set;

   -----------------------
   -- Is_Xtrace_Enabled --
   -----------------------

   function Is_Xtrace_Enabled (S : Shell_State) return Boolean is
   begin
      return S.XTrace_Enabled;
   end Is_Xtrace_Enabled;

   -----------------
   -- Leave_Scope --
   -----------------

   procedure Leave_Scope
     (Current  : in out Shell_State;
      Previous : in out Shell_State)
   is
      procedure Leave_Scope_Aux (Position : Cursor);

      ---------------------
      -- Leave_Scope_Aux --
      ---------------------

      procedure Leave_Scope_Aux (Position : Cursor) is
         V : Var_Value := Element (Position);
      begin
         if V.Scope_Owner = Current.Scope_Level and then V.Val /= null then
            Free (V.Val);
         end if;

      end Leave_Scope_Aux;
   begin

      --  Clear variable table
      Iterate (Current.Var_Table,
               Leave_Scope_Aux'Unrestricted_Access);
      Clear (Current.Var_Table);

      --  Propagate exit status
      Previous.Last_Exit_Status := Current.Last_Exit_Status;

      --  Clear positional parameters
      if Current.Pos_Params.Scope = Current.Scope_Level then
         for J in Current.Pos_Params.Table'Range loop
            Free (Current.Pos_Params.Table (J));
         end loop;
      end if;

      --  Free Trap actions
      for J in Current.Trap_Actions'Range loop
         if Current.Trap_Actions (J) /= null then
            Free (Current.Trap_Actions (J));
         end if;
      end loop;

      --  Free Current dir
      Free (Current.Current_Dir);
   end Leave_Scope;

   ------------------
   -- Resolve_Path --
   ------------------

   function Resolve_Path
     (State : Shell_State; Path : String) return String
   is
   begin
      if Is_Absolute_Path (Path) then
         return Path;
      elsif Path = "" then
         return "";
      else

         return Get_Current_Dir (State) & "/" & Path;
      end if;
   end Resolve_Path;

   -----------------------------------
   -- Restore_Positional_Parameters --
   -----------------------------------

   procedure Restore_Positional_Parameters
     (State : in out Shell_State; Pos_Params : Pos_Params_State)
   is
   begin
      --  Free if necessary previous parameters
      if State.Pos_Params.Scope = State.Scope_Level then
         for J in State.Pos_Params.Table'Range loop
            Free (State.Pos_Params.Table (J));
         end loop;
      end if;

      State.Pos_Params := Pos_Params;
   end Restore_Positional_Parameters;

   ---------------------------
   -- Save_Last_Exit_Status --
   ---------------------------

   procedure Save_Last_Exit_Status
     (State : in out Shell_State; Exit_Status : Integer)
   is
   begin
      State.Last_Exit_Status :=  Exit_Status;
   end Save_Last_Exit_Status;

   -------------------------------
   -- Get_Positional_Parameters --
   -------------------------------

   function Get_Positional_Parameters
     (State : Shell_State)
      return Pos_Params_State
   is
   begin
      return State.Pos_Params;
   end Get_Positional_Parameters;

   ---------------------
   -- Set_Current_Dir --
   ---------------------
   procedure Set_Current_Dir (State : in out Shell_State; Dir : String) is
   begin
      Free (State.Current_Dir);
      State.Current_Dir := new String'(Dir);
   end Set_Current_Dir;

   ------------------------
   -- Set_File_Expansion --
   ------------------------

   procedure Set_File_Expansion (S : in out Shell_State; Value : Boolean) is
   begin
      S.File_Expansion_Enabled := Value;
   end Set_File_Expansion;

   -------------------------------
   -- Set_Positional_Parameters --
   -------------------------------

   procedure Set_Positional_Parameters
     (State         : in out Shell_State;
      Args          : String_List;
      Free_Previous : Boolean := True)
   is
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
      if Free_Previous then
         if State.Pos_Params.Scope = State.Scope_Level then
            for J in State.Pos_Params.Table'Range loop
               Free (State.Pos_Params.Table (J));
            end loop;
         end if;
      end if;
      --  XXX missing free for the list itself.

      --  Set the new parameters
      State.Pos_Params := (Args_Copy, 0, State.Scope_Level);
   end Set_Positional_Parameters;

   ---------------------
   -- Set_Script_Name --
   ---------------------

   procedure Set_Script_Name (S : in out Shell_State; Value : String) is

      function Path_Transform (S : String) return String;
      --  Transform windows path into a unix like path

      --------------------
      -- Path_Transform --
      --------------------

      function Path_Transform (S : String) return String is
      begin
         if Directory_Separator = '\' then
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
   begin
      S.Script_Name := new String'(Path_Transform (Value));
   end Set_Script_Name;

   ---------------------
   -- Set_Trap_Action --
   ---------------------

   procedure Set_Trap_Action
     (S : in out Shell_State;
      Action : String_Access;
      Signal_Number : Integer)
   is
   begin
      if S.Trap_Actions (Signal_Number) /= null then
         Free (S.Trap_Actions (Signal_Number));
      end if;

      S.Trap_Actions (Signal_Number) := Action;
   end Set_Trap_Action;

   ---------------------
   -- Get_Trap_Action --
   ---------------------

   function Get_Trap_Action
     (S : Shell_State; Signal_Number : Integer)
      return String_Access
   is
   begin
      return S.Trap_Actions (Signal_Number);
   end Get_Trap_Action;

   -------------------
   -- Set_Var_Value --
   -------------------

   procedure Set_Var_Value
     (State  : in out Shell_State;
      Name   : String;
      Value  : String;
      Export : Boolean := False;
      Is_Env_Value : Boolean := False)
   is
      V : Var_Value := (null, null, False, State.Scope_Level);
      Is_New_Var : constant Boolean := not Contains (State.Var_Table, Name);

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
               Last_Was_Slash : Boolean := False;
               Index : Natural := S'First;
            begin
               loop
                  if S (Index) = ':' then
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := ';';
                     Is_First := True;
                     Last_Was_Slash := False;
                  elsif S (Index) = '/' then
                     if Is_First then
                        Result_Last := Result_Last + 2;
                        Result (Result_Last - 1) := 'C';
                        Result (Result_Last) := ':';
                     end if;
                     Is_First := False;
                     Last_Was_Slash := True;
                     Result_Last := Result_Last + 1;
                     Result (Result_Last) := '\';
                  elsif S (Index) = '.' then
                     if Is_First or else Last_Was_Slash then
                        if Index < S'Last then
                           if S (Index + 1) = '/' then
                              Index := Index + 1;
                              Last_Was_Slash := True;
                           elsif S (Index + 1) = ':' then
                              null;
                           else
                              Result_Last := Result_Last + 1;
                              Result (Result_Last) := S (Index);
                              Last_Was_Slash := False;
                           end if;
                        end if;
                     else
                        Result_Last := Result_Last + 1;
                        Result (Result_Last) := S (Index);
                        Last_Was_Slash := False;
                     end if;
                     Is_First := False;
                  else
                     Is_First := False;
                     Last_Was_Slash := False;
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
         V := Element (State.Var_Table, Name);

         if V.Scope_Owner = State.Scope_Level and V.Val /= null then
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

         V.Scope_Owner := State.Scope_Level;

         --  Update the variable table
         Replace (State.Var_Table, Name, V);

         --  If the variable is exported, the current env is no more valid.
         if V.Is_Exported then
            State.Is_Env_Valid := False;
         end if;
      else
         --  The variable is new
         if Is_Env_Value then
            --  We are in the context of Import_Environment.
            Include (State.Var_Table, Name,
              (new String'(Value),
               new String'(Value), Export, State.Scope_Level));
         else
            Include
              (State.Var_Table, Name,
               (new String'(Effective_Value),
                null, Export, State.Scope_Level));
         end if;

         --  Invalidate current env
         if Export then
            State.Is_Env_Valid := False;
         end if;
      end if;

   end Set_Var_Value;

   ----------------
   -- Set_Xtrace --
   ----------------

   procedure Set_Xtrace (S : in out Shell_State; Value : Boolean) is
   begin
      S.XTrace_Enabled := Value;
   end Set_Xtrace;

   ---------------------------------
   -- Shift_Positional_Parameters --
   ---------------------------------

   procedure Shift_Positional_Parameters
     (State : in out Shell_State; N : Natural; Success : out Boolean)
   is
      Shift : constant Integer := State.Pos_Params.Shift;
      Pos_Params_Size : constant Integer :=
        State.Pos_Params.Table.all'Length;
   begin
      if Shift + N > Pos_Params_Size then
         --  Shift is too large, report an error.
         Success := False;
         return;
      end if;

      State.Pos_Params.Shift := Shift + N;
      Success := True;
   end Shift_Positional_Parameters;

   ---------------
   -- Unset_Var --
   ---------------

   procedure Unset_Var (State : in out Shell_State; Name : String) is
   begin
      if Contains (State.Var_Table, Name) then
         declare
            V : Var_Value := Element (State.Var_Table, Name);
         begin
            if V.Scope_Owner = State.Scope_Level and V.Val /= null then
               --  The variable has been declared in the current context, so we
               --  can safely free the current value
               Free (V.Val);
            end if;
            Delete (State.Var_Table, Name);
         end;
      end if;
   end Unset_Var;

   --------------------------
   -- Get_Loop_Scope_Level --
   --------------------------

   function Get_Loop_Scope_Level (S : Shell_State) return Natural is
   begin
      return S.Loop_Scope_Level;
   end Get_Loop_Scope_Level;

   --------------------------
   -- Set_Loop_Scope_Level --
   --------------------------

   procedure Set_Loop_Scope_Level (S : in out Shell_State; N : Natural) is
   begin
      S.Loop_Scope_Level := N;
   end Set_Loop_Scope_Level;

end Posix_Shell.Variables;
