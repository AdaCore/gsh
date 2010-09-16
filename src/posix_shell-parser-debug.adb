with Ada.Text_IO;

package body Posix_Shell_Parser.Debug is
   ---------------------
   -- Dump_Node_Table --
   ---------------------

   procedure Dump_Node_Table is
      use Ada.Text_IO;
   begin
      for I in 1 .. Next_Node - 1 loop
         Put ("no:" & Integer'Image (I));
         Set_Col (8);
         Put (" kind: " & Node_Kind'Image (NTable (I).NKind));
         Set_Col (28);
         Put (" cmd1:" & Node_Id'Image (NTable (I).Command1));
         Set_Col (40);
         Put (" cmd2:" & Node_Id'Image (NTable (I).Command2));
         Set_Col (52);
         Put (" cmd3:" & Node_Id'Image (NTable (I).Command3));

         Set_Col (66);
         Put (" op:" & Token_Type'Image (NTable (I).Operator));
         Set_Col (76);

         if NTable (I).Rule /= null then
            Put ("word:" & NTable (I).Rule.all);
         end if;
         Set_Col (80);

         for J in 0 .. 2 loop
            Put (Integer'Image (J));
            if NTable (I).R (J).Filename = null then
               Put (">std");
            else
               Put (" "
                    & Token_Type'Image (NTable (I).R (J).Mode) & " "
                    & NTable (I).R (J).Filename.all);
            end if;
         end loop;

         Set_Col (120);
         if NTable (I).Word /= null then
            Put ("word:" & NTable (I).Word.all);


         end if;
         if NTable (I).Word_List /= null then
            for J in NTable (I).Word_List'Range loop
               Put (" " & NTable (I).Word_List (J).all);
            end loop;
         end if;
         New_Line;
      end loop;
   end Dump_Node_Table;
end Posix_Shell_Parser.Debug;
