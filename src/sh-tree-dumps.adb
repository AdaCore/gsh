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

with Ada.Text_IO; use Ada.Text_IO;
with Sh.String_Utils; use Sh.String_Utils;

package body Sh.Tree.Dumps is

   --  One function per node kind is declared
   procedure Dump_Function (N : Node);

   procedure Dump_Subshell (N : Node; Tree_Name : String);
   procedure Dump_Cmd (T : Shell_Tree; N : Node);
   procedure Dump_Assign (T : Shell_Tree; N : Node);
   procedure Dump_Pipe (N : Node);
   procedure Dump_For (T : Shell_Tree; N : Node; Tree_Name : String);
   procedure Dump_Tree (T : Shell_Tree; Name : String := "root");
   procedure Dump_Brace (N : Node; Tree_Name : String);
   procedure Dump_Block (N : Node; Tree_Name : String);

   function To_Yaml (T : Token) return String;
   --  Return string representing a token
   --
   --  @param T token to represent
   --  @return a double quoted string suitable for YAML output

   function To_Yaml (N         : Node_Id; Tree_Name : String;
                     Decl      : Boolean := False) return String;
   --  Return a node representation as a YAML reference
   --
   --  @param N the node to represent
   --  @return a string

   procedure Dump_Token_List (T : Shell_Tree; TL : Token_List);
   --  Dump a token list
   --
   --  @param T the shell tree that contains the token list
   --  @param TL the token list to dump

   -------------
   -- To_Yaml --
   -------------

   function To_Yaml (T : Token) return String
   is
      Str             : constant String := Tokens.As_String (T);
      Quoted_Str      : String (1 .. 2 * Str'Last + 2);
      Quoted_Str_Last : Natural := 0;
   begin
      --  Start with a double quote
      Quoted_Str_Last := Quoted_Str_Last + 1;
      Quoted_Str (Quoted_Str_Last) := '"';

      --  Add content with escaping of " and \
      for Index in Str'Range loop
         if Str (Index) = '\' or else Str (Index) = '"' then
            Quoted_Str_Last := Quoted_Str_Last + 1;
            Quoted_Str (Quoted_Str_Last) := '\';
         end if;

         Quoted_Str_Last := Quoted_Str_Last + 1;
         Quoted_Str (Quoted_Str_Last) := Str (Index);
      end loop;

      --  Add final double quote
      Quoted_Str_Last := Quoted_Str_Last + 1;
      Quoted_Str (Quoted_Str_Last) := '"';

      return Quoted_Str (1 .. Quoted_Str_Last);
   end To_Yaml;

   -------------
   -- To_Yaml --
   -------------

   function To_Yaml
     (N         : Node_Id;
      Tree_Name : String;
      Decl      : Boolean := False)
      return String
   is
      Prefix : constant String := (if Decl then "&" else "*");
   begin
      if Tree_Name = "root" then
         return Prefix & To_String (N);
      else
         return Prefix & Tree_Name & "_" & To_String (N);
      end if;
   end To_Yaml;

   ---------------
   -- Dup_Brace --
   ---------------

   procedure Dump_Brace (N : Node; Tree_Name : String) is
   begin
      Put_Line ("        code: " & To_Yaml (N.Brace_Code, Tree_Name));
   end Dump_Brace;

   procedure Dump_Block (N : Node; Tree_Name : String) is
   begin
      Put_Line ("        code: " & To_Yaml (N.Code, Tree_Name));
   end Dump_Block;

   ---------------------
   -- Dump_Token_List --
   ---------------------

   procedure Dump_Token_List (T : Shell_Tree; TL : Token_List) is
      Cursor   : Token_List := TL;
      Pool     : constant List_Pool := Token_List_Pool (T);
      Is_First : Boolean := True;

   begin
      Put ("[");

      while Cursor /= Null_List loop
         if not Is_First then
            Put (", ");
         end if;

         Is_First := False;
         Put (To_Yaml (Get_Element (Pool, Cursor)));
         Cursor := Next (Pool, Cursor);
      end loop;

      Put ("]");
   end Dump_Token_List;

   ----------
   -- Dump --
   ----------

   procedure Dump (T : Shell_Tree) is
      N : Node;
   begin
      for J in 1 .. Last (T.Node_Table) loop
         N := T.Node_Table.Table (J);
         if N.Kind = FUNCTION_NODE then
            Dump_Tree (N.Function_Code.all,
                       Tokens.As_String (N.Function_Name));
         end if;
      end loop;
      Dump_Tree (T);
   end Dump;

   ---------------
   -- Dump_Tree --
   ---------------

   procedure Dump_Tree (T : Shell_Tree; Name : String := "root")
   is
   begin
      Put_Line (Name & ": &" & Name);
      Put_Line ("  toplevel: " & To_String (T.Toplevel_Node));
      Put_Line ("  nodes: ");

      for J in 1 .. Last (T.Node_Table) loop
         Put_Line ("    " & To_String (J) & ": " & To_Yaml (J, Name, True));
         Dump (T, T.Node_Table.Table (J), Name);
      end loop;
   end Dump_Tree;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (T         : Shell_Tree;
      N         : Node;
      Tree_Name : String := "root")
   is
   begin
      Put_Line ("        kind: " & N.Kind'Img);
      Put_Line ("        cont_if_true: " &
                  To_Yaml (N.Cont_If_True, Tree_Name));
      Put_Line ("        cont_if_false: " &
                  To_Yaml (N.Cont_If_False, Tree_Name));

      case N.Kind is
         when FUNCTION_NODE => Dump_Function (N);
         when SUBSHELL_NODE => Dump_Subshell (N, Tree_Name);
         when CMD_NODE => Dump_Cmd (T, N);
         when ASSIGN_NODE => Dump_Assign (T, N);
         when PIPE_NODE => Dump_Pipe (N);
         when FOR_NODE => Dump_For (T, N, Tree_Name);
         when BRACE_NODE => Dump_Brace (N, Tree_Name);
         when BLOCK_NODE => Dump_Block (N, Tree_Name);
         when others => Put_Line ("unknown structure");

      end case;
   end Dump;

   --------------
   -- Dump_For --
   --------------

   procedure Dump_For (T : Shell_Tree; N : Node; Tree_Name : String) is
   begin
      Put_Line ("        var: " & To_Yaml (N.Loop_Var));
      Put ("        values: ");
      Dump_Token_List (T, N.Loop_Var_Values);
      New_Line;
      Put_Line ("        code: " & To_Yaml (N.Loop_Code, Tree_Name));
      Put_Line ("        defaul_values: " & N.Loop_Default_Values'Img);
   end Dump_For;

   ---------------
   -- Dump_Pipe --
   ---------------

   procedure Dump_Pipe (N : Node) is
   begin
      for J in N.Pipe_Childs'Range loop
         Put (N.Pipe_Childs (J)'Img);
      end loop;
      New_Line;
   end Dump_Pipe;

   -----------------
   -- Dump_Assign --
   -----------------

   procedure Dump_Assign
     (T : Shell_Tree;
      N : Node)
   is
   begin
      Put ("        assign_list: ");
      Dump_Token_List (T, N.Assign_List);
      New_Line;
   end Dump_Assign;

   --------------
   -- Dump_Cmd --
   --------------

   procedure Dump_Cmd (T : Shell_Tree; N : Node)
   is
   begin
      Put_Line ("        cmd: " & To_Yaml (N.Cmd));
      Put ("        args: ");
      Dump_Token_List (T, N.Arguments);
      New_Line;

   end Dump_Cmd;

   -------------------
   -- Dump_Function --
   -------------------

   procedure Dump_Function (N : Node) is
   begin
      Put_Line ("        name: " & To_Yaml (N.Function_Name));
      Put_Line ("        code: *" & Tokens.As_String (N.Function_Name));
   end Dump_Function;

   -------------------
   -- Dump_Subshell --
   -------------------

   procedure Dump_Subshell (N : Node; Tree_Name : String) is
   begin
      Put_Line ("        code: " & To_Yaml (N.Subshell_Code, Tree_Name));
   end Dump_Subshell;

end Sh.Tree.Dumps;
