with C.Strings;
with GNAT.Strings;
with System;
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   package CS renames C.Strings;
   package GS renames GNAT.Strings;

   function C_Element (Addr : System.Address; Idx : Integer) return Character;
   pragma Import (C, C_Element, "c_element");

   function C_List_Element (Addr : System.Address; Idx, Idx2 : Integer)
      return Character;
   pragma Import (C, C_List_Element, "c_list_element");

   procedure Assert_Element (Addr : System.Address; Str : String)
   is
   begin
      for Index in 1 .. Str'Last loop
         A.Assert (C_Element (Addr, Index - 1) = Str (Index));
      end loop;
   end Assert_Element;

   procedure Assert_Element (Addr : System.Address; Idx : Integer; Str : String)
   is
   begin
      for Index in 1 .. Str'Last loop
         A.Assert (C_List_Element (Addr, Idx, Index - 1) = Str (Index));
      end loop;
      A.Assert (C_List_Element (Addr, Idx, Str'Last) = ASCII.NUL);
   end Assert_Element;

   procedure Test_Append is
      L : CS.CList;
   begin
      CS.Append (L, "string1");
      A.Assert (CS.Length (L) = 1, "Expect Length (L) = 1");
      CS.Append (L, "");
      CS.Append (L, "");
      A.Assert (CS.Element (L, 2), "", "Check that Element (L, 2) = """"");
      begin
         if CS.Element (L, 4) = "" then
            A.Assert (False, "no 4th element. exception expected");
         end if;
      exception
         when Constraint_Error =>
            A.Assert (True);
         when others =>
            A.Assert (False, "wrong exception");
      end;
   end Test_Append;

   procedure Test_Append_List is
      GL : GS.String_List (1 .. 2) := (1 => new String'("toto"),
                                       2 => new String'("tata"));
      L : CS.CList;
   begin
      CS.Append(L, GL);
      A.Assert (CS.Length (L) = 2, "expect length 2, got:" & CS.Length (L)'Img);
      A.Assert (CS.Element (L, 1), "toto");
      A.Assert (CS.Element (L, 2), "tata");
   end Test_Append_List;

   procedure Test_As_List is
      L : CS.CList;
      GL : GS.String_List := CS.As_List(L);
   begin
      CS.Deallocate (L);
      CS.Append (L, "toto");
      CS.Append (L, "");
      declare
         GL2 : GS.String_List := CS.As_List (L);
      begin
         A.Assert (GL2'Length = 2, "expect length 2");
      end;
      CS.Deallocate (L);
   end Test_As_List;

   procedure Test_Join is
      L : CS.CList;
   begin
      A.Assert (CS.Join (L, ",", "'"), "", "Empty list join");
      CS.Append (L, "string1");
      A.Assert (CS.Join (L, ",", "'"), "'string1'",
                "single element list join");
      CS.Append (L, "string2");
      A.Assert (CS.Join (L, ",", ""), "string1,string2",
                "two elements list join");
   end Test_Join;

   procedure Test_As_C_String_Array is
      L : CS.Clist;
      Addr : System.Address;
      use type System.Address;
   begin
      A.Assert (CS.As_C_String_Array (L) = System.Null_Address);
      CS.Append (L, "string1");
      CS.Append (L, "string2");
      Addr := CS.As_C_String_Array (L);
      Assert_Element (Addr, 0, "string1");
      Assert_Element (Addr, 1, "string2");
   end Test_As_C_String_Array;

   procedure Test_As_C_Block is
      L : CS.CList;
      Addr : System.Address;
      use type System.Address;
   begin
      A.Assert (CS.As_C_String_Block (L) = System.Null_Address);
      CS.Append (L, "string1");
      CS.Append (L, "string2");
      Addr := CS.As_C_String_Block (L);
      Assert_Element (Addr, "string1" & ASCII.NUL & "string2" & ASCII.NUL & ASCII.NUL);
   end Test_As_C_Block;

   procedure Test_Long_List is
      L : CS.CList;
   begin
      for Idx in 1 .. 2047 loop
         CS.Append (L, "a");
      end loop;
      CS.Append (L, "z");
      A.Assert (CS.Element (L, 2048), "z");
   end Test_Long_List;

begin
   Test_Append;
   Test_Append_List;
   Test_As_List;
   Test_Join;
   Test_As_C_String_Array;
   Test_As_C_Block;
   Test_Long_List;
   return A.Report;
end Test;
