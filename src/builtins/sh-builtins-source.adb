------------------------------------------------------------------------------
--                                                                          --
--                                  G S H                                   --
--                                                                          --
--                       Sh.Builtins.Source                        --
--                                                                          --
--                                 B o d y                                  --
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

with Sh.Parser;                use Sh.Parser;
with Sh.Tree.Evals;            use Sh.Tree.Evals;
with Sh.Tree;                  use Sh.Tree;
with Sh.States.IO;      use Sh.States.IO;
with OS;

package body Sh.Builtins.Source is

   --------------------
   -- Source_Builtin --
   --------------------

   function Source_Builtin
     (S : in out Shell_State; Args : String_List) return Eval_Result
   is
      T                : Shell_Tree;
      Return_Code      : Eval_Result;
      Saved_Pos_Params : Pos_Params_State;
   begin
      if Args'Length = 0 then
         Error (S, ".: filename argument required");
         return (RESULT_STD, 2);
      end if;

      begin
         T := Parse_File (Resolve_Path (S, Args (Args'First).all));
         Allow_Return (T);
      exception
         when OS.OS_Error =>
            return (RESULT_STD, 1);
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
         Saved_Pos_Params := Get_Positional_Parameters (S);
         Set_Positional_Parameters
           (S, Args (Args'First + 1 .. Args'Last), False);
      end if;

      Return_Code := Eval (S, T);
      if Return_Code.Kind = RESULT_RETURN then
         Return_Code := (RESULT_STD, Return_Code.Status);
      end if;
      Free_Node (T);

      --  Restore the positional parameters if necessary.
      if Args'Length > 1 then
         Restore_Positional_Parameters (S, Saved_Pos_Params);
      end if;

      return Return_Code;
   end Source_Builtin;

end Sh.Builtins.Source;
