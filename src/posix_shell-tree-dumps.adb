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

with Ada.Text_IO; use Ada.Text_IO;

package body Posix_Shell.Tree.Dumps is

   procedure Dump_List (N : Node; Indent : Integer := 0);
   procedure Dump_If (N : Node; Indent : Integer := 0);
   procedure Dump_Function (N : Node; Indent : Integer := 0);
   procedure Dump_Subshell (N : Node; Indent : Integer := 0);
   procedure Dump_Cmd (N : Node; Indent : Integer := 0);
   procedure Dump_Assign (N : Node; Indent : Integer := 0);
   procedure Dump_Pipe (N : Node; Indent : Integer := 0);

   procedure Dump (N : Node_Id; Indent : Integer := 0) is
   begin
      Dump (Node_Table.Table (N).all, Indent);
   end Dump;

   procedure Dump (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      case N.Kind is
         when LIST_NODE => Dump_List (N, Indent);
         when IF_NODE   => Dump_If (N, Indent);
         when FUNCTION_NODE => Dump_Function (N, Indent);
         when SUBSHELL_NODE => Dump_Subshell (N, Indent);
         when CMD_NODE => Dump_Cmd (N, Indent);
         when ASSIGN_NODE => Dump_Assign (N, Indent);
         when PIPE_NODE => Dump_Pipe (N, Indent);
         when others => Put_Line ("unknown structure");

      end case;
   end Dump;

   procedure Dump_Pipe (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Dump (N.Pipe_Left,  Indent);
      Dump (N.Pipe_Right, Indent);
   end Dump_Pipe;

   procedure Dump_Assign (N : Node; Indent : Integer := 0) is
   begin

      for I in 1 .. Length (N.Assign_List) loop
         Set_Col (Positive_Count ((Indent * 4) + 1));
         Put_Line (Image (Element (N.Assign_List, I)));
      end loop;
   end Dump_Assign;

   procedure Dump_Cmd (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put (Image (N.Cmd) & " ");
      for I in 1 .. Length (N.Arguments) loop
         Put (Image (Element (N.Arguments, I)) & " ");

      end loop;
      New_Line;
   end Dump_Cmd;

   procedure Dump_Function (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line ("function " & Str (N.Function_Name));
   end Dump_Function;

   procedure Dump_If (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line ("if");
      Dump (N.Cond, Indent + 1);
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line ("then");
      Dump (N.True_Code, Indent + 1);

      if N.False_Code /= Null_Node then
         Set_Col (Positive_Count ((Indent * 4) + 1));
         Put_Line ("else");
         Dump (N.False_Code, Indent + 1);
      end if;
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line ("fi");
   end Dump_If;

   procedure Dump_List (N : Node; Indent : Integer := 0) is
   begin
      for J in N.List_Childs'Range loop
         Set_Col (Positive_Count ((Indent * 4) + 1));
         Dump (N.List_Childs (J),  Indent);
      end loop;
   end Dump_List;

   procedure Dump_Subshell (N : Node; Indent : Integer := 0) is
   begin
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line ("(");
      Dump (N.Subshell_Code, Indent + 1);
      Set_Col (Positive_Count ((Indent * 4) + 1));
      Put_Line (")");
   end Dump_Subshell;

end Posix_Shell.Tree.Dumps;
