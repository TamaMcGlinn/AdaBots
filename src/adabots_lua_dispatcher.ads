with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;

package Adabots_Lua_Dispatcher is

   type Lua_Dispatcher is new Ada.Finalization.Limited_Controlled with private;

   function Create_Lua_Dispatcher (Port : Integer) return Lua_Dispatcher;

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String;
   function Boolean_Function (T : Lua_Dispatcher; Lua_Code : String) return Boolean;
   procedure Raw_Procedure (T : Lua_Dispatcher; Lua_Code : String);

private

   task type Command_Server (Port : Integer) is
      entry Schedule_Command (Command : String);
      entry Fetch_Command (Command : out Unbounded_String);
      entry Push_Return_Value (Return_Value : String);
      entry Get_Result (Result : out Unbounded_String);
      entry Shutdown;
   end Command_Server;

   type Access_Command_Server is access Command_Server;

   type Lua_Dispatcher is new Ada.Finalization.Limited_Controlled with record
      Server : Access_Command_Server;
   end record;

   overriding procedure Finalize (T : in out Lua_Dispatcher);

end Adabots_Lua_Dispatcher;
