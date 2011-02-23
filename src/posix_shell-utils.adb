with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with System; use System;
with GNAT.OS_Lib;
with Ada.Unchecked_Conversion;


package body Posix_Shell.Utils is

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of a C string. Does check for null address
   --  (returns 0).

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access;
   --  Converts a C String to an Ada String. We could do this making use of
   --  Interfaces.C.Strings but we prefer not to import that entire package

   ------------
   -- Append --
   ------------

--     procedure Append
--       (L : in out Dynamic_String_Lists.Instance;
--        E : String_List)
--     is
--        use Dynamic_String_Lists;
--        Old_Last : constant Integer := Last (L);
--        New_Last : constant Integer := Old_Last + E'Length;
--     begin
--        Set_Last (L, New_Last);
--        L.Table (Old_Last + 1 .. New_Last) := Table_Type (E);
--     end Append;

   ---------------------
   -- C_String_Length --
   ---------------------

   function C_String_Length (S : Address) return Integer is
      function Strlen (S : Address) return Integer;
      pragma Import (C, Strlen, "strlen");
   begin
      if S = Null_Address then
         return 0;
      else
         return Strlen (S);
      end if;
   end C_String_Length;

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

   function Locate_Exec
     (Exec_Name : String) return String_Access
   is
      function Locate_Exec_On_Path (C_Exec_Name : Address) return Address;
      pragma Import (C, Locate_Exec_On_Path, "__gnat_locate_exec_on_path");

      procedure Free (Ptr : System.Address);
      pragma Import (C, Free, "free");

      C_Exec_Name  : String (1 .. Exec_Name'Length + 1);
      Path_Addr    : Address;
      Path_Len     : Integer;
      Result       : String_Access;

   begin
      C_Exec_Name (1 .. Exec_Name'Length)   := Exec_Name;
      C_Exec_Name (C_Exec_Name'Last)        := ASCII.NUL;

      Path_Addr := Locate_Exec_On_Path (C_Exec_Name'Address);
      Path_Len  := C_String_Length (Path_Addr);

      if Path_Len = 0 then
         return null;

      else
         Result := To_Path_String_Access (Path_Addr, Path_Len);
         Free (Path_Addr);

         --  Always return an absolute path name

         if not GNAT.OS_Lib.Is_Absolute_Path (Result.all) then
            declare
               Absolute_Path : constant String :=
                 GNAT.OS_Lib.Normalize_Pathname
                   (Result.all,
                    Resolve_Links => False);
            begin
               Free (Result);
               Result := new String'(Absolute_Path);
            end;
         end if;

         return Result;
      end if;
   end Locate_Exec;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Str : String) return String is
      Result : String (1 .. Str'Length);
      Last : Integer := 0;
   begin
      for J in Str'Range loop
         case Str (J) is
            when ASCII.CR => null;
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

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access
   is
      subtype Path_String is String (1 .. Path_Len);
      type    Path_String_Access is access Path_String;

      function Address_To_Access is new
        Ada.Unchecked_Conversion (Source => Address,
                              Target => Path_String_Access);

      Path_Access : constant Path_String_Access :=
                      Address_To_Access (Path_Addr);

      Return_Val  : String_Access;

   begin
      Return_Val := new String (1 .. Path_Len);

      for J in 1 .. Path_Len loop
         Return_Val (J) := Path_Access (J);
      end loop;

      return Return_Val;
   end To_Path_String_Access;

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
