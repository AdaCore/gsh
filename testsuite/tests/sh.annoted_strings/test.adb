with Test_Assert;
with SH.Annotated_Strings; use SH.Annotated_Strings;
with SH; use SH;

function Test return Integer is
   package A renames Test_Assert;

   Str1  : Annotated_String;
begin
   A.Assert (Image (Str1), "", "image of null string");
   A.Assert (Str (Str1), "", "image of null string");
   A.Assert (Length (Str1) = 0, "length of null annotated string");
   Append (Str1, "a string");
   A.Assert (Get_Element (Str1, 1) = (E_CHAR, 'a'),
             "test get_element");
   A.Assert (Length (Str1) = 8, "length of non null annotated string");
   Append (Str1, '!');
   Append (Str1, NULL_STRING);
   Append (Str1, QUOTED_NULL_STRING);
   Append (Str1, UNSPLITABLE_BEGIN);
   Append (Str1, UNSPLITABLE_END);
   Append (Str1, FIELD_SEP);

   A.Assert (Image (Str1), "a string!<N/><Q/><U></U><F/>",
             "test image of annotated string");
   declare
      Str2 : Annotated_String;
      --  declare Str2 here to force deallocation
   begin
      Append (Str2, "&");
      Append (Str1, Str2);
      Append (Str1, Str1);
      A.Assert (Str (Str1), "a string! &a string! &",
                "test string image of annoated string");
   end;

   Deallocate (Str1);
   A.Assert (Length (Str1) = 0,
             "length of annotated string after deallocation");
   return A.Report;
end Test;
