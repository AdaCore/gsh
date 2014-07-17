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

   procedure Dump_List (N : Node);
   procedure Dump_If (N : Node);
   procedure Dump_Function (N : Node);
   procedure Dump_Subshell (N : Node);
   procedure Dump_Cmd (T : Shell_Tree; N : Node);
   procedure Dump_Assign (T : Shell_Tree; N : Node);
   procedure Dump_Pipe (N : Node);
   procedure Dump_For (N : Node);

   procedure Dump (T : Shell_Tree) is
   begin
      for J in 1 .. Last (T.Node_Table) loop

            Put (J'Img & ":");
            Dump (T, T.Node_Table.Table (J));
      end loop;
   end Dump;

   procedure Dump
     (T : Shell_Tree;
      N : Node)
   is
   begin
      Put (N.Kind'Img & ": ");
      case N.Kind is
         when LIST_NODE => Dump_List (N);
         when IF_NODE   => Dump_If (N);
         when FUNCTION_NODE => Dump_Function (N);
         when SUBSHELL_NODE => Dump_Subshell (N);
         when CMD_NODE => Dump_Cmd (T, N);
         when ASSIGN_NODE => Dump_Assign (T, N);
         when PIPE_NODE => Dump_Pipe (N);
         when FOR_NODE => Dump_For (N);
         when others => Put_Line ("unknown structure");

      end case;
   end Dump;

   procedure Dump_For (N : Node) is
   begin
      Put ("var: " & Get_Token_String (N.Loop_Var) & ", ");
      Put ("values: ,");
      Put ("code:" & N.Loop_Code'Img & ", ");
      Put ("use @:" & N.Loop_Default_Values'Img);
      New_Line;
   end Dump_For;

   procedure Dump_Pipe (N : Node) is
   begin
      for J in N.Pipe_Childs'Range loop
         Put (N.Pipe_Childs (J)'Img);
      end loop;
      New_Line;
   end Dump_Pipe;

   procedure Dump_Assign
     (T : Shell_Tree;
      N : Node)
   is
      Cursor : Token_List := N.Assign_List;
      Pool   : constant List_Pool := Token_List_Pool (T);
   begin
      while Cursor /= Null_List loop
         Put (Get_Token_String (Get_Element (Pool, Cursor)));
         Put (", ");
         Cursor := Next (Pool, Cursor);
      end loop;
      New_Line;
   end Dump_Assign;

   procedure Dump_Cmd (T : Shell_Tree; N : Node)
   is
      Cursor : Token_List := N.Arguments;
      Pool   : constant List_Pool := Token_List_Pool (T);
   begin
      Put (Get_Token_String (N.Cmd) & " ");
      while Cursor /= Null_List loop
         Put (Get_Token_String (Get_Element (Pool, Cursor)) & " ");
         Cursor := Next (Pool, Cursor);
      end loop;
      New_Line;
   end Dump_Cmd;

   procedure Dump_Function (N : Node) is
   begin
      Put_Line ("function " & Get_Token_String (N.Function_Name));
   end Dump_Function;

   procedure Dump_If (N : Node) is
   begin
      Put ("if: ");
      Put (N.Cond'Img);
      Put (" then");
      Put (N.True_Code'Img);

      Put (" else");
      Put (N.False_Code'Img);
      New_Line;
   end Dump_If;

   procedure Dump_List (N : Node) is
   begin
      for J in N.List_Childs'Range loop
         Put (N.List_Childs (J)'Img);
      end loop;
      New_Line;
   end Dump_List;

   procedure Dump_Subshell (N : Node) is
   begin
      Put_Line ("code:" & N.Subshell_Code'Img);
   end Dump_Subshell;

end Posix_Shell.Tree.Dumps;
