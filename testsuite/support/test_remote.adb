------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;
with GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.Expect.TTY;
with Ada.Text_IO;
with GNATCOLL.Utils;

package body Test_Remote is

   package Expect renames GNAT.Expect;
   package OS renames GNAT.OS_Lib;
   package OS_Path renames GNAT.Directory_Operations;
   package IO renames Ada.Text_IO;

   use type GVT.FS_String;

   --------------
   -- Nickname --
   --------------

   function Nickname (Server : Local_Transport) return String is
   begin
      return "local";
   end Nickname;

   --------------
   -- Shell_FS --
   --------------

   function Shell_FS (Server : Local_Transport) return GVT.FS_Type is
   begin
      return GVT.FS_Unix;
   end Shell_FS;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   procedure Execute_Remotely
      (Server : access Local_Transport;
       Args   : GNAT.Strings.String_List;
       Status : out Boolean;
       Execution_Directory : GVT.FS_String := "")
   is
      CWD          : constant String := OS_Path.Get_Current_Dir;
      Script       : constant String := GNATCOLL.Utils.Join (" ", Args);
      Program_Name : constant String                   := "/bin/bash";
      Program_Args : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("-c"), 2 => new String'(Script));
   begin
      if Execution_Directory /= "" then
         OS_Path.Change_Dir (String (Execution_Directory));
      end if;
      OS.Spawn (Program_Name, Program_Args, Status);
      if Execution_Directory /= "" then
         OS_Path.Change_Dir (String (CWD));
      end if;
   end Execute_Remotely;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   procedure Execute_Remotely
      (Server : access Local_Transport;
       Args   : GNAT.Strings.String_List;
       Result : out GNAT.Strings.String_Access;
       Status : out Boolean;
       Execution_Directory : GVT.FS_String := "")
   is
      CWD          : constant String := OS_Path.Get_Current_Dir;
      Program_Name : constant String                   := Args (1).all;
      Program_Args : constant GNAT.Strings.String_List :=
        Args (2 .. Args'Last);
      Int_Status : aliased Integer;
   begin
      if Execution_Directory /= "" then
         OS_Path.Change_Dir (String (Execution_Directory));
      end if;
      IO.Put_Line (Program_Name);
      Result :=
        new String'
          (Expect.Get_Command_Output
             (Program_Name, Program_Args, "", Int_Status'Access, True));
      if Int_Status = 0 then
         Status := True;
      else
         Status := False;
      end if;
      if Execution_Directory /= "" then
         OS_Path.Change_Dir (String (CWD));
      end if;
   end Execute_Remotely;

   --------------------
   -- Spawn_Remotely --
   --------------------

   procedure Spawn_Remotely
      (Server     : access Local_Transport;
       Descriptor : out GNAT.Expect.Process_Descriptor_Access;
       Args       : GNAT.Strings.String_List)
   is

      Program_Name : constant String                   := Args (1).all;
      Program_Args : constant GNAT.Strings.String_List :=
        Args (2 .. Args'Last);
   begin
      Descriptor := new GNAT.Expect.TTY.TTY_Process_Descriptor;
      IO.Put_Line (Program_Name);
      Expect.Non_Blocking_Spawn (Descriptor.all, Program_Name, Program_Args);
   end Spawn_Remotely;

   -------------------
   -- Is_Configured --
   -------------------

   function Is_Configured (Config : Local_DB; Nickname : String) return Boolean
   is
   begin
      if Nickname = "local_test" then
         return True;
      else
         return False;
      end if;
   end Is_Configured;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server
      (Config   : Local_DB;
       Nickname : String) return GR.Server_Access
   is
   begin
      return new Local_Transport;
   end Get_Server;

   ---------------------
   -- Nb_Mount_Points --
   ---------------------

   function Nb_Mount_Points
      (Config   : Local_DB;
       Nickname : String) return Natural
   is
   begin
      return 1;
   end Nb_Mount_Points;

   --------------------------------
   -- Get_Mount_Point_Local_Root --
   --------------------------------

   function Get_Mount_Point_Local_Root
      (Config   : Local_DB;
       Nickname : String;
       Index    : Natural) return GVT.FS_String
   is
   begin
      return "/tmp/local_vfs";
   end Get_Mount_Point_Local_Root;

   -------------------------------
   -- Get_Mount_Point_Host_Root --
   -------------------------------

   function Get_Mount_Point_Host_Root
      (Config   : Local_DB;
       Nickname : String;
       Index    : Natural) return GVT.FS_String
   is
   begin
      return "/tmp/remote_vfs";
   end Get_Mount_Point_Host_Root;

end Test_Remote;
