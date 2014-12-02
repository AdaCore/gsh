------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
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

package body Posix_Shell.Traces is

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;
   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor;

   -----------------
   -- Open_Append --
   -----------------

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Append
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Open_Append, "__gnat_open_append");
   begin
      return C_Open_Append (Name, Fmode);
   end Open_Append;

   -----------------
   -- Open_Append --
   -----------------

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Open_Append (C_Name (C_Name'First)'Address, Fmode);
   end Open_Append;

   ---------
   -- Log --
   ---------

   procedure Log (Logger : String; Msg : String) is
      Final : constant String := Logger & ":" & Msg & ASCII.LF;
      N : Integer;
   begin
      if Enable_Traces then
         if Logger_Handler = Invalid_FD then
            Logger_Handler := Open_Append ("/tmp/gsh.out", Binary);
         end if;

         N := Write (Logger_Handler, Final'Address, Final'Length);
      end if;
   end Log;

end Posix_Shell.Traces;
