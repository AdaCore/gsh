with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Posix_Shell.Utils is

   -------------------------------
   -- Current_Working_Directory --
   -------------------------------

   function Current_Working_Directory (S : Shell_State) return String is
      Dir  : constant String := Format_Pathname (Get_Current_Dir, UNIX);
      Last : Integer := Dir'Last;
      CWD  : constant String := Get_Var_Value (S, "PWD");
      CWD_Norm : constant String := Format_Pathname
        (GNAT.OS_Lib.Normalize_Pathname (CWD), UNIX);
   begin

      --  Normally, Dir should always end with the directory separator,
      --  but this is not documented in the spec, so just make sure of
      --  this. If not, then no need to strip it, and hence we can return
      --  this path as is.
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

      if CWD_Norm = Dir (Dir'First .. Last) then
         return CWD;
      else
         return Dir (Dir'First .. Last);
      end if;
   end Current_Working_Directory;

   -----------------
   -- Integer_Not --
   -----------------

   function Integer_Not (Value : Integer) return Integer is
   begin
      if Value = 0 then
         return 1;
      else
         return 0;
      end if;
   end Integer_Not;

   function Is_Natural (S : String) return Boolean is
   begin
      if S = "" then
         return False;
      end if;

      for J in S'Range loop
         if S (J) not in '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Natural;

   -----------------
   -- Locate_Exec --
   -----------------

   Executables_Extensions : constant array (1 .. 3) of String_Access :=
     (new String'(""), new String'(".exe"), new String'(".bat"));
   --  Constant list of extensions we are looking for when searching for an
   --  executable.

   function Locate_Exec
     (S : Shell_State; Exec_Name : String) return String_Access
   is
      Current_Path : constant String := Get_Var_Value (S, "PATH");
      Path_Start : Integer := Current_Path'First;
      Result : String_Access := null;
   begin
      if Base_Name (Exec_Name) /= Exec_Name then
         --  A path is present in the required executable so don't look in the
         --  PATH. Just guess extension
         for E in Executables_Extensions'Range loop
            declare
               Ext : constant String := Executables_Extensions (E).all;
               Tentative_Path : constant String :=
                 Resolve_Path (S, Exec_Name & Ext);
            begin
               if GNAT.OS_Lib.Is_Regular_File (Tentative_Path) then
                  Result := new String'(Tentative_Path);
                  return Result;
               end if;
            end;
         end loop;
         return Result;
      end if;

      for Pos in Current_Path'Range loop
         if Current_Path (Pos) = ':' then
            --  we have a path check if our executable is there
            if Path_Start < Pos then
               for E in Executables_Extensions'Range loop
                  declare
                     Ext : constant String := Executables_Extensions (E).all;
                     Tentative_Path : constant String :=
                       Resolve_Path
                         (S, Current_Path (Path_Start .. Pos - 1)
                          & "/" & Exec_Name & Ext);
                  begin
                     if GNAT.OS_Lib.Is_Regular_File (Tentative_Path) then
                        Result := new String'(Tentative_Path);
                        return Result;
                     end if;
                  end;
               end loop;
            end if;

            Path_Start := Pos + 1;
         end if;
      end loop;
      return Result;
   end Locate_Exec;

   --------------
   -- Readline --
   --------------

   function Internal_Readline (P : chars_ptr) return chars_ptr;
   pragma Import (C, Internal_Readline, "internal_readline");

   function Readline (Prompt : String) return String is
      Result : chars_ptr := Internal_Readline (New_String (Prompt));
      Result_Str : constant String := Value (Result);
   begin
      Free (Result);
      return Result_Str;
   end Readline;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Str : String) return String is
      Result : String (1 .. Str'Length);
      Last : Integer := 0;
   begin
      for J in Str'Range loop
         case Str (J) is
            when ASCII.CR =>
               if J /= Str'Last and then Str (J + 1) /= ASCII.LF then
                  Last := Last + 1;
                  Result (Last) := Str (J);
               end if;
            when others   =>
               Last := Last + 1;
               Result (Last) := Str (J);
         end case;
      end loop;
      if Last = 0 then
         return "";
      else
         return Result (1 .. Last);
      end if;
   end Strip_CR;

   ----------------
   -- To_Integer --
   ----------------

   procedure To_Integer (S : String; V : out Integer; Valid : out Boolean) is
   begin
      --  Try the conversion. If no exception gets raised as a result,
      --  then the representation was correct.
      V := Integer'Value (S);
      Valid := True;

   exception
      when others =>
         Valid := False;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (B : Boolean) return Integer is
   begin
      if B then
         return 0;
      else
         return 1;
      end if;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   function To_String (I : Integer) return String is
      Str : constant String := Integer'Image (I);
   begin
      if I < 0 then
         return Str;
      else
         return Str (Str'First + 1 .. Str'Last);
      end if;
   end To_String;

end Posix_Shell.Utils;
