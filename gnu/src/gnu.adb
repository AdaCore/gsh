------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

package body GNU is

   function C_Fnmatch
     (Pattern : System.Address; Str : System.Address; Flags : Integer)
      return Integer;
   pragma Import (C, C_Fnmatch, "gsh_fnmatch");

   -------------
   -- Fnmatch --
   -------------

   function Fnmatch
     (Pattern     : String;
      Str         : String;
      Pathname    : Boolean := False;
      No_Escape   : Boolean := False;
      Period      : Boolean := False;
      Leading_Dir : Boolean := False;
      Ignore_Case : Boolean := False)
      return Boolean
   is
      C_Pattern : aliased String (1 .. Pattern'Length + 1);
      C_Str     : aliased String (1 .. Str'Length + 1);
      Flags     : Integer := 0;
      Result    : Integer;
   begin
      C_Pattern (1 .. Pattern'Length) := Pattern;
      C_Pattern (C_Pattern'Last) := ASCII.NUL;
      C_Str (1 .. Str'Length) := Str;
      C_Str (C_Str'Last) := ASCII.NUL;

      if Pathname then
         Flags := Flags + 1;
      end if;

      if No_Escape then
         Flags := Flags + 2;
      end if;

      if Period then
         Flags := Flags + 4;
      end if;

      if Leading_Dir then
         Flags := Flags + 8;
      end if;

      if Ignore_Case then
         Flags := Flags + 16;
      end if;

      Result := C_Fnmatch (C_Pattern'Address, C_Str'Address, Flags);
      if Result = 0 then
         return True;
      else
         return False;
      end if;

   end Fnmatch;

   -------------
   -- Regcomp --
   -------------

   procedure Regcomp
     (Regexp      : in out Regex_Type;
      Pattern     : String;
      Extended    : Boolean := False;
      Ignore_Case : Boolean := False)
   is

      function C_Regcomp
        (Regexp  : in out Regex_Type;
         Pattern : System.Address;
         Flags   : Integer)
         return Integer;
      pragma Import (C, C_Regcomp, "gsh_regcomp");

      C_Pattern : aliased String (1 .. Pattern'Length + 1);
      Flags     : Integer := 8; --  REG_NOSUB
      Status    : Integer := 0;
   begin
      C_Pattern (1 .. Pattern'Length) := Pattern;
      C_Pattern (C_Pattern'Last) := ASCII.NUL;

      if Extended then
         Flags := Flags + 1;
      end if;

      if Ignore_Case then
         Flags := Flags + 2;
      end if;

      Status := C_Regcomp (Regexp, C_Pattern'Address, Flags);

      if Status /= 0 then
         raise Invalid_Regex;
      end if;
   end Regcomp;

   -------------
   -- Regexec --
   -------------

   function Regexec
     (Regexp  : Regex_Type;
      Str     : String)
      return Boolean
   is
      function C_Regexec
        (Regexp  : System.Address;
         Str     : System.Address;
         Sub_Num : unsigned;
         Matches : System.Address;
         Flags   : Integer)
         return Integer;
      pragma Import (C, C_Regexec, "rpl_regexec");

      C_Str  : aliased String (1 .. Str'Length + 1);
      Status : Integer;
   begin
      C_Str (1 .. Str'Length) := Str;
      C_Str (C_Str'Last) := ASCII.NUL;
      Status := C_Regexec (Regexp'Address,
                           C_Str'Address, 0, System.Null_Address, 0);
      if Status = 0 then
         return True;
      else
         return False;
      end if;
   end Regexec;

end GNU;
