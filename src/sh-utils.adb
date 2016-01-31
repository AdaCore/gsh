------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                              Sh.Utils                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2010-2016, AdaCore                   --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Sh.Utils is

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
      Paths : constant String := Get_Var_Value (S, "PATH");
      Path_Start : Integer := Paths'First;
      Result : String_Access := null;

      function Get_Path (Path : String) return String;
      --  Return the corresponding path associated with an element of the PATH
      --  env variable. Basically the function returns identity or "." the
      --  element length is 0 (POSIX requires that null components in the PATH
      --  env var are handled as ".".

      function Get_File (Path : String) return String_Access;
      --  given a path, probe for file existence with the common windows
      --  executable extensions ("", ".exe" and ".bat"). Returns the path of
      --  first encountered file. Note that if no file is found then null is
      --  returned.

      --------------
      -- Get_File --
      --------------

      function Get_File (Path : String) return String_Access is
      begin
         for E in Executables_Extensions'Range loop
            declare
               Ext : constant String := Executables_Extensions (E).all;
               Win_Path : constant String := Resolve_Path (S, Path & Ext);
            begin
               if GNAT.OS_Lib.Is_Regular_File (Win_Path) then
                  return new String'(Win_Path);
               end if;
            end;
         end loop;

         --  No suitable file was found so return null
         return null;
      end Get_File;

      --------------
      -- Get_Path --
      --------------

      function Get_Path (Path : String) return String is
      begin
         if Path'Length > 0 then
            return Path & "/";
         else
            return "./";
         end if;
      end Get_Path;

   begin
      if Base_Name (Exec_Name) /= Exec_Name then
         --  A path is present in the required executable so don't look in the
         --  PATH. Just guess extension
         Result := Get_File (Exec_Name);
      else
         --  Path passed to the function is a base name so scan PATH variable
         for Pos in Paths'Range loop
            if Paths (Pos) = ':' then
               Result := Get_File
                 (Get_Path (Paths (Path_Start .. Pos - 1)) & Exec_Name);
               exit when Result /= null;
               Path_Start := Pos + 1;
            elsif Pos = Paths'Last then
               Result := Get_File
                 (Get_Path (Paths (Path_Start .. Pos)) & Exec_Name);
            end if;
         end loop;
      end if;

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

   -------------
   -- To_Long --
   -------------

   procedure To_LongLong (S : String; V : out Long_Long_Integer;
                          Valid : out Boolean) is
   begin
      --  Try the conversion. If no exception gets raised as a result,
      --  then the representation was correct.
      V := Long_Long_Integer'Value (S);
      Valid := True;

   exception
      when others =>
         Valid := False;
   end To_LongLong;

end Sh.Utils;
