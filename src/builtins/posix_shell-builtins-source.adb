------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Posix_Shell.Builtins.Source                        --
--                                                                          --
--                                 B o d y                                  --
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

with Posix_Shell.Exec;                  use Posix_Shell.Exec;
with Posix_Shell.Parser;                use Posix_Shell.Parser;
with Posix_Shell.Tree.Evals;            use Posix_Shell.Tree.Evals;
with Posix_Shell.Tree;                  use Posix_Shell.Tree;
with Posix_Shell.Variables.Output;      use Posix_Shell.Variables.Output;

package body Posix_Shell.Builtins.Source is

   --------------------
   -- Source_Builtin --
   --------------------

   function Source_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      T : Shell_Tree;
      Return_Code : Integer;
      Saved_Pos_Params : Pos_Params_State;
   begin
      if Args'Length = 0 then
         Error (S.all, ".: filename argument required");
         return 2;
      end if;

      begin
         T := Parse_File (Resolve_Path (S.all, Args (Args'First).all));
      exception
         when Buffer_Read_Error =>
            return 1;
      end;

      --  If some arguments are provided for the script, then set
      --  these arguments up. This is an extension introduced by
      --  KornShell, and POSIX says that "this is a valid extension
      --  that allows a dot script to behave identically to a function".
      --
      --  Note: If no arguments are provided, then do NOT set the arguments
      --  up, because the script should be in this case able to access
      --  the positional arguments of the parent script.
      if Args'Length > 1 then
         Saved_Pos_Params := Get_Positional_Parameters (S.all);
         Set_Positional_Parameters
           (S.all, Args (Args'First + 1 .. Args'Last), False);
      end if;

      begin
         Eval (S, T);
         Return_Code := Get_Last_Exit_Status (S.all);
         --  Tree should be freed only when getting out of context not just
         --  after sourcing the script. Otherwise we don't have access anymore
         --  to functions defined in a sourced script. ??? SHOULD BE FIXED ???
         --  Free_Node (T);
      exception
         when Shell_Return_Exception =>
            Return_Code := Get_Last_Exit_Status (S.all);
      end;

      --  Restore the positional parameters if necessary.
      if Args'Length > 1 then
         Restore_Positional_Parameters (S.all, Saved_Pos_Params);
      end if;

      return Return_Code;
   end Source_Builtin;

end Posix_Shell.Builtins.Source;
