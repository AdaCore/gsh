with Posix_Shell.Variables.Output; use Posix_Shell.Variables.Output;
with Posix_Shell.Utils; use Posix_Shell.Utils;
with Posix_Shell.String_Utils; use Posix_Shell.String_Utils;

package body Posix_Shell.Builtins.Head is

   ------------------
   -- Tail_Builtin --
   ------------------

   function Head_Builtin
     (S : Shell_State_Access;
      Args : String_List)
      return Integer
   is
      Buffer : String_Access := null;
      Current_Arg : Integer := Args'First;
      Args_Last : constant Integer := Args'Last;
      Line_Number : Integer := 10;
      Enable_Minus_N_Opt : Boolean := True;

   begin
      --  First parse options
      while Current_Arg <= Args_Last loop
         declare
            CA : constant String := Args (Current_Arg).all;
            Is_Valid : Boolean;
         begin

            if CA = "-n" then
               --  We cannot have a -<number> option after a -n
               Enable_Minus_N_Opt := False;

               Current_Arg := Current_Arg + 1;

               --  Check that we have an argument
               if Current_Arg > Args_Last then
                  Put (S.all, 2,
                       "head: option -n requires an argument" & ASCII.LF);
                  return 1;
               end if;

                  --  Check that the argument is an integer
               To_Integer (Args (Current_Arg).all, Line_Number, Is_Valid);

               if not Is_Valid then
                  Put (S.all, 2, "head: invalid context" & ASCII.LF);
                  return 1;
               end if;
            elsif CA = "--" then
               exit;
            elsif Starts_With (CA, "-") then

               --  Check that the argument is an integer
               To_Integer (CA (CA'First + 1 .. CA'Last),
                           Line_Number, Is_Valid);
               if not Is_Valid or else not Enable_Minus_N_Opt then
                  Put (S.all, 2, "head: invalid option" & ASCII.LF);
                  return 1;
               end if;

               --  Only one -<number> option is accepted
               Enable_Minus_N_Opt := False;
            else
               exit;
            end if;

            Current_Arg := Current_Arg + 1;
         end;

      end loop;

      if Current_Arg > Args'Last then
         Buffer := new String'(Read (S.all, 0));
      else
         declare
            Fd : File_Descriptor;
            Byte_Reads : Integer;
            Length : Long_Integer;
         begin
            Fd := Open_Read (Resolve_Path (S.all, Args (Current_Arg).all),
                             Binary);
            Length := File_Length (Fd);
            Buffer := new String (1 .. Integer (Length));
            Byte_Reads := Read (Fd, Buffer.all'Address, Buffer.all'Last);
            Close (Fd);
            if Byte_Reads /= Integer (Length) then
               Put (S.all, 2, "head: cannot read file");
               return 1;
            end if;
         end;
      end if;

      Put (S.all, 1, To_Line (Buffer.all, Line_Number));

      Free (Buffer);
      return 0;
   end Head_Builtin;

end Posix_Shell.Builtins.Head;
