package Adabots.Single_Turtle is

   T : Turtle := Adabots.Create_Turtle (7_112);

   procedure Turn_Right;
   procedure Turn_Left;

   function Forward return Boolean;
   function Back return Boolean;
   function Up return Boolean;
   function Down return Boolean;

   -- Digging
   function Dig_Down return Boolean;
   function Dig_Up return Boolean;
   function Dig return Boolean;

   -- Placing
   function Place return Boolean;
   function Place_Down return Boolean;
   function Place_Up return Boolean;

   -- Inventory management
   procedure Select_Slot (Slot : Turtle_Inventory_Slot);
   function Get_Item_Count (Slot : Turtle_Inventory_Slot) return Stack_Count;
   function Get_Selected_Slot return Turtle_Inventory_Slot;

   -- TODO really implement these two:
   function Get_Item_Detail return Item_Detail;
   function Get_Item_Detail (Slot : Turtle_Inventory_Slot) return Item_Detail;

   --  https://tweaked.cc/module/turtle.html#v:drop
   function Drop (Amount : Stack_Count := 64) return Boolean;
--  function DropUp (Amount : Stack_Count := 64) return Boolean;
--  function DropDown (Amount : Stack_Count := 64) return Boolean;

   function Detect return Boolean;
   function Detect_Down return Boolean;
   function Detect_Up return Boolean;

   --  these procedures assert that the function of the same name returned true

   procedure Forward;
   procedure Back;
   procedure Up;
   procedure Down;

   procedure Dig_Down;
   procedure Dig_Up;
   procedure Dig;

   procedure Place;
   procedure Place_Down;
   procedure Place_Up;

   procedure Drop (Amount : Stack_Count := 64);

   -- these procedures don't care what the result is

   procedure Maybe_Dig_Down;
   procedure Maybe_Dig_Up;
   procedure Maybe_Dig;
   procedure Maybe_Place;
   procedure Maybe_Place_Down;
   procedure Maybe_Place_Up;

end Adabots.Single_Turtle;
