with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Adabots_Lua_Dispatcher;

package Adabots is

   type Turtle is new Ada.Finalization.Limited_Controlled with private;
   type Turtle_Inventory_Slot is range 1 .. 16;
   type Stack_Count is range 0 .. 64;
   type Item_Detail is record
      Count : Stack_Count;
      Name  : Ada.Strings.Unbounded.Unbounded_String;
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

   type Command_Computer is
     new Ada.Finalization.Limited_Controlled with private;

   function Create_Command_Computer return Command_Computer;
   function Create_Command_Computer (Port : Integer) return Command_Computer;

   type Material is (Grass, Planks);

   type Location (X_Size : Positive; Y_Size : Positive; Z_Size : Positive) is
     private;

   function Create_Location
     (X : String; Y : String; Z : String) return Location;

   procedure Set_Block
     (C : Command_Computer; X : String; Y : String; Z : String; B : Material);

   procedure Set_Block (C : Command_Computer; L : Location; B : Material);

private

   type Turtle is new Ada.Finalization.Limited_Controlled with record
      Dispatcher : Adabots_Lua_Dispatcher.Lua_Dispatcher;
   end record;

   function Parse_Item_Details (Table : String) return Item_Detail;

   type Command_Computer is new Ada.Finalization.Limited_Controlled with record
      Dispatcher : Adabots_Lua_Dispatcher.Lua_Dispatcher;
   end record;

   function Is_Valid_Location_Component (S : String) return Boolean;

   type Coordinate_Component (String_Size : Integer) is record
      Value : String (1 .. String_Size);
   end record with
      Dynamic_Predicate => Is_Valid_Location_Component (Value);

   type Location (X_Size : Positive; Y_Size : Positive; Z_Size : Positive)
   is record
      X : Coordinate_Component (X_Size);
      Y : Coordinate_Component (Y_Size);
      Z : Coordinate_Component (Z_Size);
   end record;

end Adabots;
