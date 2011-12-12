with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Utils; use Posix_Shell.Utils;

package body Posix_Shell.Builtins.Test is

   --  list of binary operators supported by test builtin
   type Binary_Op is
     (NULL_BIN_OP,           --  null
      UNSUPPORTED_BINARY_OP,
      EQUAL_OP,              --  =
      NOT_EQUAL_OP,          --  !=
      IEQUAL_OP,             --  -eq
      GREATER_THAN_OP,       --  -gt
      GREATER_EQUAL_THAN_OP, --  -ge
      LESS_THAN_OP,          --  -lt
      LESS_EQUAL_THAN_OP,    --  -le
      INOT_EQUAL_OP          --  -ne
     );

   --  list of unary operators supported by test builtin
   type Unary_Op is
     (NULL_UNARY_OP,         --  null
      UNSUPPORTED_UNARY_OP,
      IS_DIR_OP,             --  -d
      IS_FILE_OP,            --  -f
      IS_FILE_OR_DIR_OP,     --  -e
      IS_RFILE_OP,           --  -r
      NONZERO_STRING_OP,     --  -n
      ZERO_STRING_OP,        --  -z
      IS_NON_EMPTY_FILE,     --  -s
      IS_WFILE_OP            --  -w
     );

   type Binop_Access is access function (Left, Right : Integer) return Boolean;

   function Eval_Binop (S : Shell_State_Access;
                        Left, Right : String_Access;
                        Binop : Binop_Access) return Integer;
   --  A function that converts Left and Right to integers, and evaluates
   --  the Binop function using these values. Return 0 if Binop returned
   --  True, 1 otherwise.
   --
   --  If either of Left or Right were not valid images of decimal integers,
   --  then report a descriptive error message, and return 2.

   function Equal (Left, Right : Integer) return Boolean;
   --  A wrapper around "=". The purpose of this function is to be
   --  able to take its 'Address.

   function Not_Equal (Left, Right : Integer) return Boolean;
   --  A wrapper around "/=". The purpose of this function is to be
   --  able to take its 'Address.

   function Greater_Than (Left, Right : Integer) return Boolean;
   --  A wrapper around ">". The purpose of this function is to be
   --  able to take its 'Address.

   function Greater_Equal (Left, Right : Integer) return Boolean;
   --  A wrapper around ">=". The purpose of this function is to be
   --  able to take its 'Address.

   function Less_Than (Left, Right : Integer) return Boolean;
   --  A wrapper around "<". The purpose of this function is to be
   --  able to take its 'Address.

   function Less_Equal (Left, Right : Integer) return Boolean;
   --  A wrapper around "<=". The purpose of this function is to be
   --  able to take its 'Address.

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Integer) return Boolean is
   begin
      return Left = Right;
   end Equal;

   ----------------
   -- Eval_Binop --
   ----------------

   function Eval_Binop (S : Shell_State_Access;
                        Left, Right : String_Access;
                        Binop : Binop_Access) return Integer
   is
      Left_V, Right_V : Integer;
      Success : Boolean;
   begin
      To_Integer (Left.all, Left_V, Success);
      if not Success then
         Error (S.all, "test: " & Left.all & ": integer expression expected");
         return 2;
      end if;

      To_Integer (Right.all, Right_V, Success);
      if not Success then
         Error (S.all, "test: " & Right.all & ": integer expression expected");
         return 2;
      end if;

      if Binop (Left_V, Right_V) then
         return 0;
      else
         return 1;
      end if;
   end Eval_Binop;

   -------------------
   -- Greater_Equal --
   -------------------

   function Greater_Equal (Left, Right : Integer) return Boolean is
   begin
      return Left >= Right;
   end Greater_Equal;

   ------------------
   -- Greater_Than --
   ------------------

   function Greater_Than (Left, Right : Integer) return Boolean is
   begin
      return Left > Right;
   end Greater_Than;

   ----------------
   -- Less_Equal --
   ----------------

   function Less_Equal (Left, Right : Integer) return Boolean is
   begin
      return Left <= Right;
   end Less_Equal;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Integer) return Boolean is
   begin
      return Left < Right;
   end Less_Than;

   ---------------
   -- Not_Equal --
   ---------------

   function Not_Equal (Left, Right : Integer) return Boolean is
   begin
      return Left /= Right;
   end Not_Equal;

   ------------------
   -- Test_Builtin --
   ------------------

   function Test_Builtin
     (S : Shell_State_Access; Args : String_List) return Integer
   is
      Current_Pos : Integer := Args'First;
      --  Position of the string currently parsed in Args.

      function Get_Bin_Op (Str : String) return Binary_Op;
      --  Given a string return the corresponding binary operator. If the
      --  string does not correspond to any binrary operator return the
      --  NULL_BIN_OP.

      function Get_Unary_Op (Str : String) return Unary_Op;
      pragma Inline (Get_Unary_Op);
      --  Idem with Unary operators.

      function Eval
        (Op    : Binary_Op;
         Left  : String_Access;
         Right : String_Access)
         return Integer;
      pragma Inline (Eval);
      --  Eval a binary operator expression.

      function Eval (Op : Unary_Op; Param : String_Access) return Integer;
      --  Idem with unary operators.

      -- Parser functions --
      function Parse_Primary_Expr return Integer;
      pragma Inline (Parse_Primary_Expr);

      function Parse_Not_Expr return Integer;
      function Parse_And_Expr return Integer;
      function Parse_Or_Expr return Integer;

      ----------
      -- Eval --
      ----------

      function Eval
        (Op : Binary_Op;
         Left : String_Access;
         Right : String_Access)
         return Integer
      is
      begin
         case Op is
            when EQUAL_OP =>
               return To_Integer (Left.all = Right.all);
            when NOT_EQUAL_OP =>
               return To_Integer (Left.all /= Right.all);
            when GREATER_THAN_OP =>
               return Eval_Binop (S, Left, Right, Greater_Than'Access);
            when GREATER_EQUAL_THAN_OP =>
               return Eval_Binop (S, Left, Right, Greater_Equal'Access);
            when LESS_THAN_OP =>
               return Eval_Binop (S, Left, Right, Less_Than'Access);
            when LESS_EQUAL_THAN_OP =>
               return Eval_Binop (S, Left, Right, Less_Equal'Access);
            when IEQUAL_OP =>
               return Eval_Binop (S, Left, Right, Equal'Access);
            when INOT_EQUAL_OP =>
               return Eval_Binop (S, Left, Right, Not_Equal'Access);
            when UNSUPPORTED_BINARY_OP =>
               --  This binary operator is unsupported, so just return
               --  an arbitrary non-zero value.
               return 2;
            when NULL_BIN_OP =>
               --  Should never happen.
               pragma Assert (False);
               return 255;
         end case;
      end Eval;

      ----------
      -- Eval --
      ----------

      function Eval (Op : Unary_Op; Param : String_Access) return Integer is
         Fd : File_Descriptor;
         Result : Boolean;

         function Is_File (Filename : String) return String;
         --  If the file exist then return its name. Otherwise return ""

         -------------
         -- Is_File --
         -------------

         function Is_File (Filename : String) return String is
            Res_Path : constant String := Resolve_Path (S.all, Filename);
         begin

            if Path_Separator = ';' and then Filename = "/dev/null" then
               --  On Windows there is no /dev/null returns its equivalent
               --  which is NUL file
               return "NUL";
            end if;

            if Is_Regular_File (Res_Path) or Is_Directory (Res_Path) then
               return Res_Path;
            end if;

            if Path_Separator = ';' then

               --  we are on windows.
               --  First check if the .exe exist
               if Is_Regular_File (Res_Path & ".exe") then
                  return Res_Path & ".exe";
               end if;

            end if;
            return "";
         end Is_File;

      begin
         case Op is
            when IS_DIR_OP  =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     return To_Integer (Is_Directory (Param.all));
                  else
                     return To_Integer (False);
                  end if;
               end;
            when IS_FILE_OR_DIR_OP =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     return To_Integer ((Is_Directory (Real_File) or else
                       Is_Regular_File (Real_File)) and then
                       Real_File /= "NUL");
                  else
                     return To_Integer (False);
                  end if;
               end;
            when IS_FILE_OP =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     return To_Integer (Is_Regular_File (Real_File) and then
                                        Real_File /= "NUL");
                  else
                     return To_Integer (False);
                  end if;
               end;
            when IS_NON_EMPTY_FILE =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     Fd := Open_Read (Real_File, Binary);
                     if File_Length (Fd) > 0 then
                        Result := True;
                     else
                        Result := False;
                     end if;
                     Close (Fd);
                     return To_Integer (Result);
                  else
                     return To_Integer (False);
                  end if;
               end;
            when IS_RFILE_OP =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     return To_Integer (Is_Readable_File (Real_File));
                  else
                     return To_Integer (False);
                  end if;
               end;

            when IS_WFILE_OP =>
               declare
                  Real_File : constant String := Is_File (Param.all);
               begin
                  if Real_File /= "" then
                     return To_Integer (Is_Writable_File (Real_File));
                  else
                     return To_Integer (False);
                  end if;
               end;
            when NONZERO_STRING_OP =>
               return To_Integer (Param'Length /= 0);
            when ZERO_STRING_OP =>
               return To_Integer (Param'Length = 0);
            when UNSUPPORTED_UNARY_OP =>
               --  This unary operator is unsupported, so just return
               --  an arbitrary non-zero value.
               return 2;
            when NULL_UNARY_OP =>
               --  Should never happen.
               pragma Assert (False);
               return 255;
         end case;
      end Eval;

      ----------------
      -- Get_Bin_Op --
      ----------------

      function Get_Bin_Op (Str : String) return Binary_Op is
      begin
         if Str = "=" then
            return EQUAL_OP;
         elsif Str = "!=" then
            return NOT_EQUAL_OP;
         elsif Str = "-gt" then
            return GREATER_THAN_OP;
         elsif Str = "-ge" then
            return GREATER_EQUAL_THAN_OP;
         elsif Str = "-lt" then
            return LESS_THAN_OP;
         elsif Str = "-le" then
            return LESS_EQUAL_THAN_OP;
         elsif Str = "-eq" then
            return IEQUAL_OP;
         elsif Str = "-ne" then
            return INOT_EQUAL_OP;
         end if;

         if Str = "-nt" or else Str = "-ot" then
            Warning
              (S.all, "test: " & Str & ": binary operator not supported yet");
            return UNSUPPORTED_BINARY_OP;
         end if;

         return NULL_BIN_OP;
      end Get_Bin_Op;

      ------------------
      -- Get_Unary_Op --
      ------------------

      function Get_Unary_Op (Str : String) return Unary_Op is
      begin
         if Str = "-d" then
            return IS_DIR_OP;
         elsif Str = "-f" then
            return IS_FILE_OP;
         elsif Str = "-x" then
            return IS_RFILE_OP;
         elsif Str = "-r" then
            return IS_RFILE_OP;
         elsif Str = "-w" then
            return IS_WFILE_OP;
         elsif Str = "-n" then
            return NONZERO_STRING_OP;
         elsif Str = "-s" then
            return IS_NON_EMPTY_FILE;
         elsif Str = "-z" then
            return ZERO_STRING_OP;
         elsif Str = "-e" then
            return IS_FILE_OR_DIR_OP;
         end if;

         if Str = "-b" or else
            Str = "-c" or else
            Str = "-e" or else
            Str = "-g" or else
            Str = "-G" or else
            Str = "-h" or else
            Str = "-k" or else
            Str = "-L" or else
            Str = "-O" or else
            Str = "-p" or else
            Str = "-S" or else
            Str = "-t" or else
            Str = "-u" or else
            Str = "-x"
         then
            Warning (S.all,
                     "test: " & Str & ": unary operator not supported yet");
            return UNSUPPORTED_UNARY_OP;
         end if;

         return NULL_UNARY_OP;
      end Get_Unary_Op;

      --------------------
      -- Parse_And_Expr --
      --------------------

      function Parse_And_Expr return Integer is
         Result : Integer;
      begin
         Result := Parse_Not_Expr;
         if Result /= 0 then
            return Result;
         elsif Current_Pos > Args'Last then
            return 0;
         end if;

         if Args (Current_Pos).all = "-a" then
            Current_Pos := Current_Pos + 1;

            if Current_Pos > Args'Last then
               Error (S.all,
                      "test: argument expected after -a binary operator");
               --  This error message is different from what bash is
               --  printing. But doing the same thing as bash is not trivial
               --  and I think our message is slightly better anyway
               --  (bash prints "argument expected" or "unary opeator
               --  expected" depending on the situation).
               return 2;
            end if;

            return Parse_And_Expr;
         else
            return Result;
         end if;
      end Parse_And_Expr;

      --------------------
      -- Parse_Not_Expr --
      --------------------

      function Parse_Not_Expr return Integer is
      begin
         if Args (Current_Pos).all = "!" then

            Current_Pos := Current_Pos + 1;

            if Current_Pos > Args'Last then
               return 0;
            end if;

            return Integer_Not (Parse_Primary_Expr);
         else
            return Parse_Primary_Expr;
         end if;
      end Parse_Not_Expr;

      -------------------
      -- Parse_Or_Expr --
      -------------------

      function Parse_Or_Expr return Integer is
         Result : Integer;
      begin
         Result := Parse_And_Expr;

         if Result = 0 then
            return 0;
         elsif Current_Pos > Args'Last then
            return Result;
         end if;

         if Args (Current_Pos).all = "-o" then
            Current_Pos := Current_Pos + 1;

            if Current_Pos > Args'Last then
               Error (S.all,
                      "test: argument expected after -o binary operator");
               --  This error message is different from what bash is
               --  printing. But doing the same thing as bash is not trivial
               --  and I think our message is slightly better anyway
               --  (bash prints "argument expected" or "unary opeator
               --  expected" depending on the situation).
               return 2;
            end if;

            return Parse_Or_Expr;
         end if;

         return Result;
      end Parse_Or_Expr;

      ------------------------
      -- Parse_Primary_Expr --
      ------------------------

      function Parse_Primary_Expr return Integer is
         Current_Bin_Op : Binary_Op := NULL_BIN_OP;
         Current_Unary_Op : Unary_Op := NULL_UNARY_OP;
      begin
         if Current_Pos + 2 <= Args'Last then
            Current_Bin_Op := Get_Bin_Op (Args (Current_Pos + 1).all);
            if Current_Bin_Op /= NULL_BIN_OP then
               Current_Pos := Current_Pos + 3;
               return Eval (Current_Bin_Op,
                            Args (Current_Pos - 3),
                            Args (Current_Pos - 1));
            end if;
         end if;

         if Current_Pos + 1 <= Args'Last then
            Current_Unary_Op := Get_Unary_Op (Args (Current_Pos).all);
            if Current_Unary_Op /= NULL_UNARY_OP then
               Current_Pos := Current_Pos + 2;
               return Eval (Current_Unary_Op,
                            Args (Current_Pos - 1));
            end if;
         end if;

         Current_Pos := Current_Pos + 1;
         return To_Integer (Args (Current_Pos - 1).all /= "");
      end Parse_Primary_Expr;

   begin
      --  special case describe in Posix. If test has 0 arguments then return
      --  false (non zero value).
      if Args'Length = 0 then
         return 1;
      end if;

      return Parse_Or_Expr;
   end Test_Builtin;

end Posix_Shell.Builtins.Test;