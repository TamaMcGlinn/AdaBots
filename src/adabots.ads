with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Adabots_Lua_Dispatcher;
with Adabots_Nodetypes;

package Adabots is

   type Turtle is new Ada.Finalization.Limited_Controlled with private;
   type Turtle_Inventory_Slot is range 1 .. 16;
   type Stack_Count is range 0 .. 64;
   type Positive_Stack_Count is range 1 .. 64;
   type Item_Detail is record
      Count : Stack_Count;
      Name  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Default_Port : constant := 7_112;

   function Ask_User_For_Port return Integer;
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
   function Get_Item_Count (T : Turtle; Slot : Turtle_Inventory_Slot) return Stack_Count;
   function Get_Selected_Slot (T : Turtle) return Turtle_Inventory_Slot;

   -- TODO really implement these two:
   function Get_Item_Detail (T : Turtle) return Item_Detail;
   function Get_Item_Detail (T : Turtle; Slot : Turtle_Inventory_Slot) return Item_Detail;

   --  https://tweaked.cc/module/turtle.html#v:drop
   function Drop (T : Turtle; Amount : Stack_Count := 64) return Boolean;
--  function DropUp (T : Turtle; Amount : Stack_Count := 64) return Boolean;
--  function DropDown (T : Turtle; Amount : Stack_Count := 64) return Boolean;

   function Detect (T : Turtle) return Boolean;
   function Detect_Down (T : Turtle) return Boolean;
   function Detect_Up (T : Turtle) return Boolean;
   function Detect_Left (T : Turtle) return Boolean;
   function Detect_Right (T : Turtle) return Boolean;

   function Inspect (T : Turtle) return Adabots_Nodetypes.Node;
   function Inspect_Down (T : Turtle) return Adabots_Nodetypes.Node;
   function Inspect_Up (T : Turtle) return Adabots_Nodetypes.Node;

   function Suck (T : Turtle; Amount : Stack_Count := 64) return Boolean;
   function Suck_Down (T : Turtle; Amount : Stack_Count := 64) return Boolean;
   function Suck_Up (T : Turtle; Amount : Stack_Count := 64) return Boolean;

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

   function Craft (T : Turtle; Amount : Positive_Stack_Count := 1) return Boolean;
   procedure Craft (T : Turtle; Amount : Positive_Stack_Count := 1);

   procedure Drop (T : Turtle; Amount : Stack_Count := 64);

   --   these procedures don't care what the result is

   procedure Maybe_Dig_Down (T : Turtle);
   procedure Maybe_Dig_Up (T : Turtle);
   procedure Maybe_Dig (T : Turtle);
   procedure Maybe_Place (T : Turtle);
   procedure Maybe_Place_Down (T : Turtle);
   procedure Maybe_Place_Up (T : Turtle);

   procedure Suck (T : Turtle; Amount : Stack_Count := 64);
   procedure Suck_Down (T : Turtle; Amount : Stack_Count := 64);
   procedure Suck_Up (T : Turtle; Amount : Stack_Count := 64);

   type Command_Computer is new Ada.Finalization.Limited_Controlled with private;

   function Create_Command_Computer return Command_Computer;
   function Create_Command_Computer (Port : Integer) return Command_Computer;

   type Material is
     (Grass,
      Planks,
      Air,
      Glass,
      Ice,
      Gold_Block,
      Sand,
      Bedrock,
      Stone);

   type Relative_Location is record
      X_Offset : Integer := 0;
      Y_Offset : Integer := 0;
      Z_Offset : Integer := 0;
   end record;

   function Image (P : Relative_Location) return String is
     (P.X_Offset'Image & ", " & P.Y_Offset'Image & ", " & P.Z_Offset'Image);

   function "+" (A, B : Relative_Location) return Relative_Location;
   function "-" (A, B : Relative_Location) return Relative_Location;

   type Absolute_Location is record
      X : Integer := 0;
      Y : Integer := 0;
      Z : Integer := 0;
   end record;

   function "+" (A, B : Absolute_Location) return Absolute_Location;

   function "+" (A : Absolute_Location; B : Relative_Location) return Absolute_Location;

   function Set_Block (C : Command_Computer; L : Relative_Location; B : Material) return Boolean;

   procedure Maybe_Set_Block (C : Command_Computer; L : Relative_Location; B : Material);

   procedure Set_Cube
     (C : Command_Computer; First : Relative_Location; Last : Relative_Location; B : Material);

   function Get_Block_Info (C : Command_Computer; L : Absolute_Location) return Material;

private

   type Turtle is new Ada.Finalization.Limited_Controlled with record
      Dispatcher : Adabots_Lua_Dispatcher.Lua_Dispatcher;
   end record;

   function Parse_Item_Details (Table : String) return Item_Detail;

   type Command_Computer is new Ada.Finalization.Limited_Controlled with record
      Dispatcher : Adabots_Lua_Dispatcher.Lua_Dispatcher;
   end record;

end Adabots;
