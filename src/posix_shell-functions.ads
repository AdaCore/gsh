with GNAT.Strings; use GNAT.Strings;

with Posix_Shell.Tree; use Posix_Shell.Tree;
with Posix_Shell.Variables; use Posix_Shell.Variables;

package Posix_Shell.Functions is

   procedure Register_Function (Name : String; Tree : Shell_Tree_Access);
   --  Register the given Node as a new function.

   function Is_Function (Name : String) return Boolean;
   --  Return True if a function of the given name has been defined.

   procedure Execute_Function
     (State : Shell_State_Access;
      Name  : String;
      Args  : String_List);
   --  Execute the given function with the provide arguments, and
   --  return the function exit status.
   --
   --  The behavior is unspecified if the function has not been
   --  previously defined.

end Posix_Shell.Functions;
