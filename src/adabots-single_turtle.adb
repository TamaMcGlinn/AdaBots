package body Adabots.Single_Turtle is

   procedure Turn_Right is
   begin
      T.Turn_Right;
   end Turn_Right;

   procedure Turn_Left is
   begin
      T.Turn_Left;
   end Turn_Left;

   function Forward return Boolean is
   begin
      return T.Forward;
   end Forward;

   function Back return Boolean is
   begin
      return T.Back;
   end Back;

   function Up return Boolean is
   begin
      return T.Up;
   end Up;

   function Down return Boolean is
   begin
      return T.Down;
   end Down;

   -- Digging
   function Dig_Down return Boolean is
   begin
      return T.Dig_Down;
   end Dig_Down;

   function Dig_Up return Boolean is
   begin
      return T.Dig_Up;
   end Dig_Up;

   function Dig return Boolean is
   begin
      return T.Dig;
   end Dig;

   -- Placing
   function Place return Boolean is
   begin
      return T.Place;
   end Place;

   function Place_Down return Boolean is
   begin
      return T.Place_Down;
   end Place_Down;

   function Place_Up return Boolean is
   begin
      return T.Place_Up;
   end Place_Up;

   -- Inventory management
   procedure Select_Slot (Slot : Turtle_Inventory_Slot) is
   begin
      T.Select_Slot (Slot);
   end Select_Slot;

   function Get_Item_Count (Slot : Turtle_Inventory_Slot) return Stack_Count is
   begin
      return T.Get_Item_Count (Slot);
   end Get_Item_Count;

   function Get_Selected_Slot return Turtle_Inventory_Slot is
   begin
      return T.Get_Selected_Slot;
   end Get_Selected_Slot;

   -- TODO really implement these two:
   function Get_Item_Detail return Item_Detail is
   begin
      return T.Get_Item_Detail;
   end Get_Item_Detail;

   function Get_Item_Detail (Slot : Turtle_Inventory_Slot) return Item_Detail
   is
   begin
      return T.Get_Item_Detail (Slot);
   end Get_Item_Detail;

   --  https://tweaked.cc/module/turtle.html#v:drop
   function Drop (Amount : Stack_Count := 64) return Boolean is
   begin
      return T.Drop (Amount);
   end Drop;

--  function DropUp (Amount : Stack_Count := 64) return Boolean;
--  function DropDown (Amount : Stack_Count := 64) return Boolean;

   function Detect return Boolean is
   begin
      return T.Detect;
   end Detect;

   function Detect_Down return Boolean is
   begin
      return T.Detect_Down;
   end Detect_Down;

   function Detect_Up return Boolean is
   begin
      return T.Detect_Up;
   end Detect_Up;

   --  these procedures assert that the function of the same name returned true

   procedure Forward is
   begin
      T.Forward;
   end Forward;

   procedure Back is
   begin
      T.Back;
   end Back;

   procedure Up is
   begin
      T.Up;
   end Up;

   procedure Down is
   begin
      T.Down;
   end Down;

   procedure Dig_Down is
   begin
      T.Dig_Down;
   end Dig_Down;

   procedure Dig_Up is
   begin
      T.Dig_Up;
   end Dig_Up;

   procedure Dig is
   begin
      T.Dig;
   end Dig;

   procedure Place is
   begin
      T.Place;
   end Place;

   procedure Place_Down is
   begin
      T.Place_Down;
   end Place_Down;

   procedure Place_Up is
   begin
      T.Place_Up;
   end Place_Up;

   procedure Drop (Amount : Stack_Count := 64) is
   begin
      T.Drop (Amount);
   end Drop;

   -- these procedures don't care what the result is

   procedure Maybe_Dig_Down is
   begin
      T.Maybe_Dig_Down;
   end Maybe_Dig_Down;

   procedure Maybe_Dig_Up is
   begin
      T.Maybe_Dig_Up;
   end Maybe_Dig_Up;

   procedure Maybe_Dig is
   begin
      T.Maybe_Dig;
   end Maybe_Dig;

   procedure Maybe_Place is
   begin
      T.Maybe_Place;
   end Maybe_Place;

   procedure Maybe_Place_Down is
   begin
      T.Maybe_Place_Down;
   end Maybe_Place_Down;

   procedure Maybe_Place_Up is
   begin
      T.Maybe_Place_Up;
   end Maybe_Place_Up;

end Adabots.Single_Turtle;
