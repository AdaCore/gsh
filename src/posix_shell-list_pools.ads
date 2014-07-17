with GNAT.Dynamic_Tables;
with Posix_Shell.Lexer; use Posix_Shell.Lexer;

package Posix_Shell.List_Pools is

   subtype Token_List is Natural;
   type List_Pool is private;

   type Node is private;


   Null_List : constant Token_List := 0;

   procedure Append
     (Pool : in out List_Pool; Source : in out Token_List; Item : Token);

   procedure Append
     (Pool : in out List_Pool; Source : in out Token_List; Item : Token_List);

   function Prepend
     (Pool   : in out List_Pool;
      Source : Token_List;
      Item   : Token)
     return Token_List;

   procedure Deallocate (Pool : in out List_Pool; Source : Token_List);

   function Next (Pool : List_Pool; Source : Token_List) return Token_List;

   function Get_Element (Pool : List_Pool; Source : Token_List) return Token;

   function Is_Empty (Pool : List_Pool; Source : Token_List) return Boolean;

   function New_Pool return List_Pool;

private

   type Node is record
      T      : Token := Null_Token;
      Next   : Token_List := 0;
      Last   : Token_List := 0;
   end record;

   package List_Pools is new GNAT.Dynamic_Tables
     (Node, Token_List, 1, 4, 100);
   type List_Pool is new List_Pools.Instance;

end Posix_Shell.List_Pools;
