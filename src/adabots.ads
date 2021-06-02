with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with AWS.Response;
with AWS.Status;

package Adabots is
   type Turtle is new Ada.Finalization.Limited_Controlled with private;
   function Create_Turtle (Port : Integer := 7_112) return Turtle;

   function Turn_Right (T : Turtle) return Boolean;
   function Turn_Left (T : Turtle) return Boolean;
   function Forward (T : Turtle) return Boolean;
   function Back (T : Turtle) return Boolean;
   function Up (T : Turtle) return Boolean;
   function Down (T : Turtle) return Boolean;
   function Dig_Down (T : Turtle) return Boolean;
   function Dig_Up (T : Turtle) return Boolean;
   function Dig (T : Turtle) return Boolean;

private

   task type Command_Server (Port : Integer) is
      entry Schedule_Command (Command : String);
      entry Fetch_Command (Command : out Unbounded_String);
      entry Push_Return_Value (Return_Value : String);
      entry Get_Result (Result : out Unbounded_String);
      entry Shutdown;
   end Command_Server;

   type Turtle is new Ada.Finalization.Limited_Controlled with record
      Server : access Command_Server;
   end record;

   overriding procedure Finalize (T : in out Turtle);

   function Raw_Function (T : Turtle; Lua_Code : String) return String;

   function Boolean_Function (T : Turtle; Lua_Code : String) return Boolean;

end Adabots;
