with Ada.Strings.Unbounded;

package Adabots_Lua_Dispatcher is

   type Lua_Dispatcher is private;

   function Create_Lua_Dispatcher (Workspace_ID : Ada.Strings.Unbounded.Unbounded_String; Bot_Name : Ada.Strings.Unbounded.Unbounded_String) return Lua_Dispatcher;

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String;
   function Boolean_Function (T : Lua_Dispatcher; Lua_Code : String) return Boolean;
   procedure Raw_Procedure (T : Lua_Dispatcher; Lua_Code : String);

private

   type Lua_Dispatcher is record
      Workspace_ID : Ada.Strings.Unbounded.Unbounded_String;
      Bot_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Adabots_Lua_Dispatcher;
