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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;

package GNU is

   function Fnmatch
     (Pattern     : String;
      Str         : String;
      Pathname    : Boolean := False;
      No_Escape   : Boolean := False;
      Period      : Boolean := False;
      Leading_Dir : Boolean := False;
      Ignore_Case : Boolean := False)
      return Boolean;
   --  Match Str against Pattern (fnmatch posix function)
   --
   --  @param Pattern
   --      the pattern to be matched. See POSIX standard for the format used.
   --  @param Str the string to be checked
   --  @param Pathname if True then no wildcard can match /.
   --  @param No_Escape if True \ cannot be used to quote special characters
   --  @param Period if True then leading . is matched only explicitely
   --  @param Leading_Dir if True then ignore '/...' after a match
   --  @param Ignore_Case if True then match is done without case sensitiveness
   --  @return True if Str match Pattern, False otherwise

   type Regex_Type is private;

   procedure Regcomp
     (Regexp      : in out Regex_Type;
      Pattern     : String;
      Extended    : Boolean := False;
      Ignore_Case : Boolean := False);
   --  Compile a regular expression (BRE or ERE)
   --
   --  @param Regexp structure to store the compiled expression
   --  @param Pattern string representing the regular expression
   --  @param Extended if True then ERE are used, otherwise BRE
   --  @param Ignore_Case if True then casing is ignored
   --  @raise Invalid_Regex is raised in case of error

   procedure Regfree (Regexp : in out Regex_Type);
   pragma Import (C, Regfree, "rpl_regfree");

   function Regexec (Regexp  : Regex_Type;
                     Str     : String)
                     return Boolean;

   Invalid_Regex : exception;

private

   type Regex_Type is record
      buffer           : System.Address;
      allocated        : aliased unsigned_long;
      used             : aliased unsigned_long;
      syntax           : aliased unsigned_long;
      fastmap          : Strings.chars_ptr;
      translate        : System.Address;
      re_nsub          : aliased unsigned;
      can_be_null      : Extensions.Unsigned_1;
      regs_allocated   : Extensions.Unsigned_2;
      fastmap_accurate : Extensions.Unsigned_1;
      no_sub           : Extensions.Unsigned_1;
      not_bol          : Extensions.Unsigned_1;
      not_eol          : Extensions.Unsigned_1;
      newline_anchor   : Extensions.Unsigned_1;
   end record;
   pragma Convention (C_Pass_By_Copy, Regex_Type);
   pragma Pack (Regex_Type);

end GNU;
