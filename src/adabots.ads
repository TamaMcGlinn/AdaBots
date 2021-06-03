with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with AWS.Response;
with AWS.Status;

package Adabots is
   type Turtle is new Ada.Finalization.Limited_Controlled with private;
   type Turtle_Inventory_Slot is range 1 .. 16;

   function Create_Turtle (Port : Integer := 7_112) return Turtle;

   procedure Turn_Right (T : Turtle);
   procedure Turn_Left (T : Turtle);

   function Forward (T : Turtle) return Boolean;
   function Back (T : Turtle) return Boolean;
   function Up (T : Turtle) return Boolean;
   function Down (T : Turtle) return Boolean;

   function Dig_Down (T : Turtle) return Boolean;
   function Dig_Up (T : Turtle) return Boolean;
   function Dig (T : Turtle) return Boolean;

   function Place (T : Turtle) return Boolean;
   function Place_Down (T : Turtle) return Boolean;
   function Place_Up (T : Turtle) return Boolean;

   procedure Select_Slot (T : Turtle; Slot : Turtle_Inventory_Slot);

   --  these procedures assert that the function of the same name returned true

   procedure Forward (T : Turtle);
   procedure Back (T : Turtle);
   procedure Up (T : Turtle);
   procedure Down (T : Turtle);

   procedure Dig_Down (T : Turtle);
   procedure Dig_Up (T : Turtle);
   procedure Dig (T : Turtle);

   procedure Place (T : Turtle);
   procedure Place_Down (T : Turtle);
   procedure Place_Up (T : Turtle);

   --   these procedures don't care what the result is

   procedure Maybe_Dig_Down (T : Turtle);
   procedure Maybe_Dig_Up (T : Turtle);
   procedure Maybe_Dig (T : Turtle);
   procedure Maybe_Place (T : Turtle);
   procedure Maybe_Place_Down (T : Turtle);
   procedure Maybe_Place_Up (T : Turtle);

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

   function String_Function (T : Turtle; Lua_Code : String) return String;

   procedure Turtle_Procedure (T : Turtle; Lua_Code : String);

end Adabots;
