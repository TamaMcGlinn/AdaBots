with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Adabots_Lua_Dispatcher;

package Adabots_Command_Computers is

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

end Adabots_Command_Computers;
