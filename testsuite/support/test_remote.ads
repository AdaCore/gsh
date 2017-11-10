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

--  The package provides remote support using only local resources. This
--  allows to test remote functionalities without the need for a remote host.

with GNAT.Expect;
with GNAT.Strings;
with GNATCOLL.Remote;
with GNATCOLL.VFS_Types;
with GNATCOLL.Remote.DB;

package Test_Remote is

   package GR renames GNATCOLL.Remote;
   package GRDB renames GNATCOLL.Remote.DB;
   package GVT renames GNATCOLL.VFS_Types;

   --  Declare local transport protocol (basically spawn /bin/bash)
   type Local_Transport is new GR.Server_Record with null record;

   function Nickname (Server : Local_Transport) return String;
   function Shell_FS (Server : Local_Transport) return GVT.FS_Type;

   procedure Execute_Remotely
      (Server               : access Local_Transport;
       Args                 : GNAT.Strings.String_List;
       Status               : out Boolean;
       Execution_Directory  : GVT.FS_String := "");

   procedure Execute_Remotely
      (Server : access Local_Transport;
       Args   : GNAT.Strings.String_List;
       Result : out GNAT.Strings.String_Access;
       Status : out Boolean;
       Execution_Directory : GVT.FS_String := "");

   procedure Spawn_Remotely
      (Server     : access Local_Transport;
       Descriptor : out GNAT.Expect.Process_Descriptor_Access;
       Args       : GNAT.Strings.String_List);

   --  Declare local remote database which holds only one host nickname
   --  called local_test
   type Local_DB is new GRDB.Remote_Db_Interface with null record;

   function Is_Configured
      (Config   : Local_DB;
       Nickname : String)
      return Boolean;

   function Get_Server
      (Config   : Local_DB;
       Nickname : String)
      return GR.Server_Access;

   function Nb_Mount_Points
      (Config   : Local_DB;
       Nickname : String)
      return Natural;

   function Get_Mount_Point_Local_Root
      (Config   : Local_DB;
       Nickname : String;
       Index    : Natural)
      return GVT.FS_String;

   function Get_Mount_Point_Host_Root
      (Config   : Local_DB;
       Nickname : String;
       Index    : Natural) return GVT.FS_String;

end Test_Remote;
