with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;

package Adabots is
   type Turtle is new Ada.Finalization.Limited_Controlled with private;
   type Turtle_Inventory_Slot is range 1 .. 16;
   type Stack_Count is range 0 .. 64;
   type Item_Detail is record
      Count : Stack_Count;
      Name  : Unbounded_String;
   end record;

   function Create_Turtle return Turtle;
   function Create_Turtle (Port : Integer) return Turtle;

   -- Movement
   procedure Turn_Right (T : Turtle);
   procedure Turn_Left (T : Turtle);

   function Forward (T : Turtle) return Boolean;
   function Back (T : Turtle) return Boolean;
   function Up (T : Turtle) return Boolean;
   function Down (T : Turtle) return Boolean;

   -- Digging
   function Dig_Down (T : Turtle) return Boolean;
   function Dig_Up (T : Turtle) return Boolean;
   function Dig (T : Turtle) return Boolean;

   -- Placing
   function Place (T : Turtle) return Boolean;
   function Place_Down (T : Turtle) return Boolean;
   function Place_Up (T : Turtle) return Boolean;

   -- Inventory management
   procedure Select_Slot (T : Turtle; Slot : Turtle_Inventory_Slot);
   function Get_Item_Count
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Stack_Count;
   function Get_Selected_Slot (T : Turtle) return Turtle_Inventory_Slot;

   -- TODO really implement these two:
   function Get_Item_Detail (T : Turtle) return Item_Detail;
   function Get_Item_Detail
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Item_Detail;

   --  https://tweaked.cc/module/turtle.html#v:drop
   function Drop (T : Turtle; Amount : Stack_Count := 64) return Boolean;
--  function DropUp (T : Turtle; Amount : Stack_Count := 64) return Boolean;
--  function DropDown (T : Turtle; Amount : Stack_Count := 64) return Boolean;

   function Detect (T : Turtle) return Boolean;
   function Detect_Down (T : Turtle) return Boolean;
   function Detect_Up (T : Turtle) return Boolean;

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

   procedure Drop (T : Turtle; Amount : Stack_Count := 64);

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

   function Parse_Item_Details (Table : String) return Item_Detail;

end Adabots;
