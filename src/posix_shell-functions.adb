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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Posix_Shell.Exec; use Posix_Shell.Exec;
with Posix_Shell.Tree.Evals; use Posix_Shell.Tree.Evals;

package body Posix_Shell.Functions is

   package Function_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String,
         Element_Type => Shell_Tree,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   use Function_Maps;

   Function_Map : Function_Maps.Map;
   --  A table that maps function names to their associated Node.

   ----------------------
   -- Execute_Function --
   ----------------------

   procedure Execute_Function
     (State : Shell_State_Access;
      Name  : String;
      Args : String_List)
   is
      Function_Tree : constant Shell_Tree :=
        Element (Function_Map, Name);
      Saved_Pos_Params : constant Pos_Params_State :=
        Get_Positional_Parameters (State.all);

   begin
      Set_Positional_Parameters (State.all, Args, False);

      Eval (State, Function_Tree);
      Restore_Positional_Parameters (State.all, Saved_Pos_Params);

   exception
      when Shell_Exit_Exception =>
         Restore_Positional_Parameters (State.all, Saved_Pos_Params);
         raise;
   end Execute_Function;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Name : String) return Boolean is
   begin
      return Contains (Function_Map, Name);
   end Is_Function;

   -----------------------
   -- Register_Function --
   -----------------------

   procedure Register_Function (Name : String; Tree : Shell_Tree) is
   begin
      Include (Function_Map, Name, Tree);
   end Register_Function;

end Posix_Shell.Functions;
