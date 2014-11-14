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

with Interfaces.C; use Interfaces.C;
with System;

package Lua is

   type Lua_Return_Code is
     (LUA_OK,
      LUA_YIELD,
      LUA_ERRRUN,
      LUA_ERRSYNTAX,
      LUA_ERRMEM,
      LUA_ERRGCMM,
      LUA_ERRERR,
      LUA_ERRFILE);
   --  Status types

   type Lua_Type is
     (LUA_TNONE,
      LUA_TNIL,
      LUA_TBOOLEAN,
      LUA_TLIGHTUSERDATA,
      LUA_TNUMBER,
      LUA_TSTRING,
      LUA_TTABLE,
      LUA_TFUNCTION,
      LUA_TUSERDATA,
      LUA_TTHREAD);
   --  Lua types

   type Lua_State is new System.Address;
   --  Lua state is the structure containing the state of the interpreter.

   type Lua_User_Data is new System.Address;
   type Lua_Light_User_Data is new System.Address;

   type Lua_Function is access function (State : Lua_State) return Integer;

   subtype Lua_Integer is ptrdiff_t;
   subtype Lua_Unsigned is unsigned;
   subtype Lua_Float is Long_Float;
   subtype Lua_Index is Integer;

   Lua_Type_Error     : exception;
   Lua_Override_Error : exception;
   Lua_Error          : exception;

   ---------------------
   -- State Functions --
   ---------------------

   function New_State return Lua_State;
   --  Create a new state using the default memory allocator. See Lua.Advanced
   --  in case you want to use custom allocators.

   procedure Close (State : Lua_State);
   --  Destroys all objects in the given Lua state (calling the corresponding
   --  garbage - collection metamethods, if any) and frees all dynamic memory
   --  used by this state. On several platforms, you may not need to call this
   --  function, because all resources are naturally released when the host
   --  program ends. On the other hand, long - running programs that create
   --  multiple states, such as daemons or web servers, might need to close
   --  states as soon as they are not needed.

   ---------------------
   --  Load functions --
   ---------------------

   function Load_File
     (State    : Lua_State;
      Filename : String;
      Mode     : String := "")
      return Lua_Return_Code;
   --  Loads a file as a Lua chunk. This function uses Load to load the
   --  chunk in the file named filename. If filename is NULL, then it loads
   --  from the standard input. The first line in the file is ignored if
   --  it starts with a #.
   --
   --  The string mode works as in function Load.
   --
   --  This function returns the same results as Load, but it has an extra
   --  error code LUA_ERRFILE if it cannot open / read the file or the file
   --  has a wrong mode.
   --
   --  As lua_load, this function only loads the chunk; it does not run it.

   procedure Load_File
     (State    : Lua_State;
      Filename : String;
      Mode     : String := "");
   --  Same as previous function except that Lua_Error exception is raised in
   --  LUA_OK is not returned.

   procedure Load_String
     (State : Lua_State;
      Str   : String);
   --  Load a string and raise Lua_Error in case of error

   procedure Open_Libs (State : Lua_State);
   --  Opens all standard Lua libraries into the given state.

   ----------------------
   --  Stack Functions --
   ----------------------

   function Get_Top (State : Lua_State) return Lua_Index;
   --  Returns the index of the top element in the stack. Because indices start
   --  at 1, this result is equal to the number of elements in the stack (and
   --  so 0 means an empty stack).

   procedure Set_Top
     (State : Lua_State;
      Index : Lua_Index);
   --  Accepts any index, or 0, and sets the stack top to this index. If the
   --  new top is larger than the old one, then the new elements are filled
   --  with nil. If index is 0, then all stack elements are removed.

   function Check_Stack (State : Lua_State; N : Integer) return Boolean;
   --  Ensures that there are at least extra free stack slots in the stack. It
   --  returns False if it cannot fulfill the request, because it would cause
   --  the stack to be larger than a fixed maximum size (typically at least a
   --  few thousand elements) or because it cannot allocate memory for the new
   --  stack size. This function never shrinks the stack; if the stack is
   --  already larger than the new size, it is left unchanged.

   procedure Pop
     (State : Lua_State;
      N     : Integer := 1);
   --  Pops n elements from the stack.

   procedure Push_Value (State : Lua_State; Index : Lua_Index);
   --  Pushes a copy of the element at the given index onto the stack.

   procedure Remove (State : Lua_State; Index : Lua_Index);
   --  Removes the element at the given valid index, shifting down the elements
   --  above this index to fill the gap. This function cannot be called with a
   --  pseudo - index, because a pseudo - index is not an actual stack
   --  position.

   procedure Insert (State : Lua_State; Index : Lua_Index);
   --  Moves the top element into the given valid index, shifting up the
   --  elements above this index to open space. This function cannot be called
   --  with a pseudo - index, because a pseudo - index is not an actual stack
   --  position.

   procedure Replace (State : Lua_State; Index : Lua_Index);
   --  Moves the top element into the given valid index without shifting any
   --  element (therefore replacing the value at the given index), and then
   --  pops the top element.

   procedure Copy
     (State : Lua_State;
      From  : Integer;
      To    : Integer);
   --  Moves the element at index from into the valid index to without
   --  shifting any element (therefore replacing the value at that position).

   function Absolute_Index
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_Index;
   --  Converts the acceptable index idx into an absolute index (that is, one
   --  that does not depend on the stack top).

   --------------------------
   -- Convertion Functions --
   --------------------------

   --  The following functions read objects from the Lua stack and return their
   --  Ada corresponding value.

   function To_Ada
     (State   : Lua_State;
      Index   : Lua_Index)
      return Lua_Float;
   --  Converts the Lua value at the given index to the Ada type Lua_Float
   --  The Lua value must be a number or a string convertible to a number
   --  otherwise, raise Lua_Type_Error.

   function To_Ada
     (State   : Lua_State;
      Index   : Lua_Index)
      return Lua_Integer;
   --  Converts the Lua value at the given index to the Ada type Lua_Integer
   --  The Lua value must be a number or a string convertible to a number
   --  otherwise, raise Lua_Type_Error.

   function To_Ada
     (State  : Lua_State;
      Index  : Lua_Index)
      return Lua_Unsigned;
   --  Converts the Lua value at the given index to the Ada type Lua_Unsigned
   --  The Lua value must be a number or a string convertible to a number
   --  otherwise, raise Lua_Type_Error.

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Boolean;
   --  Converts the Lua value at the given index to a boolean value.
   --  Like all tests in Lua, To_Boolean returns true for any Lua value
   --  different from false and nil; otherwise it returns false. (If you want
   --  to accept only actual boolean values, use Is_Boolean to test the
   --  value's type.)

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_Function;
   --  Converts a value at the given index to a C function. That value must be
   --  a C function; otherwise, returns NULL.

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_User_Data;
   --  If the value at the given index is a full userdata, returns its block
   --  address. If the value is a light userdata, returns its pointer.
   --  Otherwise, returns NULL.

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_State;
   --  Converts the value at the given index to a Lua thread (represented as
   --  Lua_State). This value must be a thread; otherwise, the function
   --  returns NULL.

   function To_Ada
     (State : Lua_State;
      Index : Lua_Index)
      return String;
   --  Converts the Lua value at the given index to a Ada string. The Lua value
   --  must be a string or a number; otherwise, the function raise
   --  Lua_Type_Error. If the value is a number, then lua_tolstring also
   --  changes the actual value in the stack to a string. (This change
   --  confuses lua_next when lua_tolstring is applied to keys during a table
   --  traversal.)

   --------------------
   -- Push Functions --
   --------------------

   procedure Push (State : Lua_State);
   --  Pushes a nil value onto the stack.

   procedure Push (State : Lua_State; Data : Lua_Float);
   --  Pushes a lua float onto the stack.

   procedure Push (State : Lua_State; Data : Lua_Integer);
   --  Pushes a number with value n onto the stack.

   procedure Push (State : Lua_State; Data : Lua_Unsigned);
   --  Pushes a number with value n onto the stack.

   procedure Push (State : Lua_State; Data : String);
   --  Push a string on the stack

   procedure Push_Closure
     (State : Lua_State;
      Fun   : Lua_Function;
      Closure_Size : Integer := 0);

   --  Pushes a new C closure onto the stack.
   --
   --  When a C function is created, it is possible to associate some values
   --  with it, thus creating a C closure (see §4.4); these values are then
   --  accessible to the function whenever it is called. To associate values
   --  with a C function, first these values should be pushed onto the stack
   --  (when there are multiple values, the first value is pushed first). Then
   --  lua_pushcclosure is called to create and push the C function onto the
   --  stack, with the argument n telling how many values should be associated
   --  with the function. lua_pushcclosure also pops these values from the
   --  stack.

   --  The maximum value for n is 255.

   --  When n is zero, this function creates a light C function, which is just
   --  a pointer to the C function. In that case, it never throws a memory
   --  error.

   procedure Push (State : Lua_State; Data : Boolean);
   --  Pushes a boolean value with value b onto the stack.

   procedure Push (State : Lua_State; Data : Lua_Light_User_Data);
   --  Pushes a light userdata onto the stack. Userdata represent C values in
   --  Lua. A light userdata represents a pointer, a void * . It is a value
   --  (like a number) : you do not create it, it has no individual metatable,
   --  and it is not collected (as it was never created). A light userdata is
   --  equal to "any" light userdata with the same C address.

   --------------------------
   -- Type Check Functions --
   --------------------------

   function Is_Number (State : Lua_State; Index : Lua_Index) return Boolean;
   --  Returns 1 if the value at the given index is a number or a string
   --  convertible to a number, and 0 otherwise.

   function Is_String (State : Lua_State; Index : Lua_Index) return Boolean;
   --  Returns 1 if the value at the given index is a string or a number
   --  (which is always convertible to a string), and 0 otherwise.

   function Is_Function (State : Lua_State; Index : Lua_Index) return Boolean;
   --  Returns 1 if the value at the given index is a C function, and 0
   --  otherwise.

   function Is_User_Data (State : Lua_State; Index : Lua_Index) return Boolean;
   --  Returns 1 if the value at the given index is a userdata (either full or
   --  light), and 0 otherwise.

   function Get_Type
     (State : Lua_State;
      Index : Lua_Index)
      return Lua_Type;
   --  Returns the type of the value in the given valid index, or LUA_TNONE
   --  for a non - valid (but acceptable) index.

   function Type_Name
     (State : Lua_State;
      Index : Lua_Index)
      return String;
   --  Returns the name of the type encoded by the value tp, which must be
   --  one the values returned by lua_type.

   ----------------
   -- Operations --
   ----------------

   type Lua_Compare_Op is
     (LUA_OPEQ,
      LUA_OPLT,
      LUA_OPLE);

   function Compare
     (State    : Lua_State;
      Index1   : Lua_Index;
      Index2   : Lua_Index;
      Operator : Lua_Compare_Op)
      return Boolean;

   procedure Concat (State : Lua_State; N : Integer);
   --  Concatenates the n values at the top of the stack, pops them, and leaves
   --  the result at the top. If n is 1, the result is the single value on the
   --  stack (that is, the function does nothing); if n is 0, the result is the
   --  empty string. Concatenation is performed following the usual semantics
   --  of Lua.

   type Lua_Arith_Op is
     (LUA_OPADD,
      LUA_OPSUB,
      LUA_OPMUL,
      LUA_OPDIV,
      LUA_OPMOD,
      LUA_OPPOW,
      LUA_OPUNM);

   procedure Arith (State : Lua_State; Operator : Lua_Arith_Op);
   --  Performs an arithmetic operation over the two values (or one, in the
   --  case of negation) at the top of the stack, with the value at the top
   --  being the second operand, pops these values, and pushes the result of
   --  the operation. The function follows the semantics of the corresponding
   --  Lua operator (that is, it may call metamethods).

   ---------------------
   -- Table Functions --
   ---------------------

   function Next (State : Lua_State; Index : Lua_Index) return Boolean;
   --  Pops a key from the stack, and pushes a key value pair from the table
   --  at the given index (the "next" pair after the given key). If there are
   --  no more elements in the table, then lua_next returns False (and pushes
   --  nothing).

   procedure Get_Table (State : Lua_State; Index : Lua_Index);
   --  Pushes onto the stack the value t[k], where t is the value at the given
   --  index and k is the value at the top of the stack.
   --
   --  This function pops the key from the stack (putting the resulting value
   --  in its place). As in Lua, this function may trigger a metamethod for the
   --  "index" event

   procedure Set_Table
     (State : Lua_State;
      Index : Lua_Index);
   --  Does the equivalent to t[k] = v, where t is the value at the given
   --  index, v is the value at the top of the stack, and k is the value just
   --  below the top.
   --
   --  This function pops both the key and the value from the stack. As in Lua,
   --  this function may trigger a metamethod for the "newindex" event

   procedure Get_Field
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String);
   --  Pushes onto the stack the value t[k], where t is the value at the given
   --  index. As in Lua, this function may trigger a metamethod for the "index"
   --  event

   procedure Set_Field
     (State    : Lua_State;
      Index    : Lua_Index;
      Name     : String;
      Override : Boolean := True);
   --  Does the equivalent to t[k] = v, where t is the value at the given
   --  index and v is the value at the top of the stack.
   --
   --  This function pops the value from the stack. As in Lua, this function
   --  may trigger a metamethod for the "newindex" event

   procedure Set_Field
     (State    : Lua_State;
      Index    : Lua_Index;
      Name     : String;
      Fun      : Lua_Function;
      Override : Boolean := True);

   procedure Create_Table
     (State       : Lua_State;
      N_Seq_Elmts : Integer := 0;
      N_Elmts     : Integer := 0);

   procedure Raw_Get (State : Lua_State; Index : Lua_Index);
   --  Similar to lua_gettable, but does a raw access (i.e., without
   --  metamethods).

   procedure Raw_Geti
     (State : Lua_State;
      Index : Lua_Index;
      N     : Integer);
   --  Pushes onto the stack the value t[n], where t is the table at the given
   --  index. The access is raw; that is, it does not invoke metamethods.

   -------------
   -- Threads --
   -------------

   procedure X_Move
     (From_State : Lua_State;
      To_State   : Lua_State;
      N          : Integer);
   --  Exchange values between different threads of the same state.
   --
   --  This function pops n values from the stack from, and pushes them onto
   --  the stack to.

   function New_Thread (State : Lua_State) return Lua_State;
   --  Creates a new thread, pushes it on the stack, and returns a pointer to
   --  a lua_State that represents this new thread. The new thread returned by
   --  this function shares with the original thread its global environment,
   --  but has an independent execution stack.
   --
   --  There is no explicit function to close or to destroy a thread. Threads
   --  are subject to garbage collection, like any Lua object.

   function Push_Thread (State : Lua_State) return Integer;
   --  Pushes the thread represented by L onto the stack. Returns 1 if this
   --  thread is the main thread of its state.

   function Resume
     (State  : Lua_State;
      Thread : Lua_State;
      N_Args : Integer)
      return Integer;
   --  Starts and resumes a coroutine in a given thread.

   ------------------------
   -- Global Environment --
   ------------------------

   procedure Get_Global
     (State : Lua_State;
      Name  : String);
   --  Pushes onto the stack the value of the global name.

   procedure Set_Global
     (State : Lua_State;
      Name  : String);
   --  Pops a value from the stack and sets it as the new value of global name.

   procedure Register_Function
     (State : Lua_State;
      Name  : String;
      Fun   : Lua_Function);
   --  Helper function to register a function inside lua state. Name is the
   --  name in Lua global environment with which the function is associated to.
   --  To ease create of hierarchies in the global environment, if Name
   --  contains '.' then a hierarchy using Lua tables is created. For example:
   --
   --     Register (S, "a.b.c", My_Lua_Function'Access);
   --
   --  will create a global table called "a". The table "a" will contain a
   --  table at index "b" and this last table will contain one element "b" set
   --  to our function. Note that an error will be raised in case you try to
   --  register twice at the same location.

   ------------
   -- Others --
   ------------

   function At_Panic (State : Lua_State;
                      Fun : Lua_Function)
                      return Lua_Function;

   function Version (State : Lua_State) return access Lua_Float;

   function Raw_Len (State : Lua_State; Index : Lua_Index) return size_t;
   --  Returns the raw "length" of the value at the given index: for strings,
   --  this is the string length; for tables, this is the result of the length
   --  operator ('#') with no metamethods; for userdata, this is the size of
   --  the block of memory allocated for the userdata; for other values,
   --  it is 0.

   function Raw_Equal
     (State : Lua_State;
      Index1 : Lua_Index;
      Index2 : Lua_Index) return int;
   pragma Import (C, Raw_Equal, "lua_rawequal");
   --  Returns 1 if the two values in indices index1 and index2 are
   --  primitively equal (that is, without calling metamethods). Otherwise
   --  returns 0. Also returns 0 if any of the indices are non valid.

   function New_User_Data
     (State : Lua_State;
      Size  : size_t)
      return System.Address;

   function Get_Metatable
     (State : Lua_State;
      Index : Lua_Index) return int;
   pragma Import (C, Get_Metatable, "lua_getmetatable");

   procedure Get_User_Value (State : Lua_State; Index : Lua_Index);
   pragma Import (C, Get_User_Value, "lua_getuservalue");
   --  Pushes onto the stack the Lua value associated with the userdata at
   --  the given index. This Lua value must be a table or nil.

   procedure Raw_Set (State : Lua_State; Index : Lua_Index);
   --  Similar to lua_settable, but does a raw assignment (i.e.,
   --  without metamethods).

   procedure Raw_Seti
     (State : Lua_State;
      Index : Lua_Index;
      N     : Integer);
   --  Does the equivalent of t[n] = v, where t is the table at the given
   --  index and v is the value at the top of the stack.
   --
   --  This function pops the value from the stack. The assignment is raw;
   --  that is, it does not invoke metamethods.

   function Set_Metatable (State : Lua_State; Index : Lua_Index) return int;
   --  Pops a table from the stack and sets it as the new metatable for
   --  the value at the given index.

   procedure Set_User_Value (State : Lua_State;
                             Index : Lua_Index);
   --  Pops a table or nil from the stack and sets it as the new value
   --  associated to the userdata at the given index.

   procedure Call
     (State    : Lua_State;
      Nargs    : Integer := 0;
      NResults : Integer := 0;
      Context  : Integer := 0;
      Cont_Fun : Lua_Function := null);

   function Get_Context
     (State   : Lua_State;
      Context : Integer)
      return int;
   pragma Import (C, Get_Context, "lua_getctx");

   function PCall
     (State    : Lua_State;
      Nargs    : Integer := 0;
      NResults : Integer := 0;
      Err_Fun  : Integer := 0;
      Context  : Integer := 0;
      Cont_Fun : Lua_Function := null)
      return Lua_Return_Code;
   pragma Import (C, PCall, "lua_pcallk");

   procedure PCall
     (State    : Lua_State;
      Nargs    : Integer := 0;
      NResults : Integer := 0;
      Err_Fun  : Integer := 0;
      Context  : Integer := 0;
      Cont_Fun : Lua_Function := null);
   --  Same as PCall function except that a Lua_Error exception is raised in
   --  case of error.

   function Yield
     (State    : Lua_State;
      NResults : Integer;
      Context  : Integer;
      Cont_Fun : Lua_Function := null) return int;
   pragma Import (C, Yield, "lua_yieldk");

   function Status (State : Lua_State) return Lua_Return_Code;

   function Error (State : Lua_State) return Integer;
   --  Generates a Lua error. The error message (which can actually be a Lua
   --  value of any type) must be on the stack top. This function does a long
   --  jump, and therefore never returns.

   procedure Len (State : Lua_State; Index : Lua_Index);

private

   pragma Convention (C, Lua_Return_Code);
   for Lua_Type use
     (LUA_TNONE          => -1,
      LUA_TNIL           => 0,
      LUA_TBOOLEAN       => 1,
      LUA_TLIGHTUSERDATA => 2,
      LUA_TNUMBER        => 3,
      LUA_TSTRING        => 4,
      LUA_TTABLE         => 5,
      LUA_TFUNCTION      => 6,
      LUA_TUSERDATA      => 7,
      LUA_TTHREAD        => 8);
   pragma Convention (C, Lua_Type);
   pragma Convention (C, Lua_Function);
   pragma Convention (C, Lua_Compare_Op);
   pragma Convention (C, Lua_Arith_Op);

   pragma Import (C, New_User_Data, "lua_newuserdata");
   pragma Import (C, Raw_Seti, "lua_rawseti");
   pragma Import (C, Raw_Set, "lua_rawset");

   pragma Import (C, Call, "lua_callk");
   pragma Import (C, Set_Table, "lua_settable");
   pragma Import (C, Create_Table, "lua_createtable");
   pragma Import (C, Raw_Geti, "lua_rawgeti");
   pragma Import (C, Raw_Get, "lua_rawget");
   pragma Import (C, Push_Closure, "lua_pushcclosure");
   pragma Import (C, Set_Metatable, "lua_setmetatable");
   pragma Import (C, At_Panic, "lua_atpanic");
   pragma Import (C, Version, "lua_version");
   pragma Import (C, Status, "lua_status");
   pragma Import (C, Error, "lua_error");
   pragma Import (C, Len, "lua_len");
   pragma Import (C, Resume, "lua_resume");
   pragma Import (C, Push_Thread, "lua_pushthread");
   pragma Import (C, New_Thread, "lua_newthread");
   pragma Import (C, X_Move, "lua_xmove");
   pragma Import (C, Get_Type, "lua_type");
   pragma Import (C, Get_Table, "lua_gettable");
   pragma Import (C, Set_User_Value, "lua_setuservalue");
   pragma Import (C, New_State, "luaL_newstate");
   pragma Import (C, Close, "lua_close");
   pragma Import (C, Open_Libs, "luaL_openlibs");
   pragma Import (C, Get_Top, "lua_gettop");
   pragma Import (C, Set_Top, "lua_settop");
   pragma Import (C, Push_Value, "lua_pushvalue");
   pragma Import (C, Remove, "lua_remove");
   pragma Import (C, Insert, "lua_insert");
   pragma Import (C, Replace, "lua_replace");
   pragma Import (C, Copy, "lua_copy");
   pragma Import (C, Absolute_Index, "lua_absindex");
   pragma Inline (Check_Stack);
   pragma Inline (To_Ada);
   pragma Import (C, Concat, "lua_concat");
   pragma Import (C, Arith, "lua_arith");
   pragma Import (C, Raw_Len, "lua_rawlen");
end Lua;
