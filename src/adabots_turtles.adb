package body Adabots_Turtles is
   use Adabots_Lua_Dispatcher;

   --  public:

   function Create_Turtle return Turtle is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Dispatcher => Create_Lua_Dispatcher);
   end Create_Turtle;

   function Create_Turtle (Port : Integer) return Turtle is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Dispatcher => Create_Lua_Dispatcher (Port));
   end Create_Turtle;

   procedure Turn_Right (T : Turtle) is
   begin
      Raw_Procedure (T.Dispatcher, "turtle.turnRight()");
   end Turn_Right;

   procedure Turn_Left (T : Turtle) is
   begin
      Raw_Procedure (T.Dispatcher, "turtle.turnLeft()");
   end Turn_Left;

   function Forward (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.forward()");
   end Forward;

   function Back (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.back()");
   end Back;

   function Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.up()");
   end Up;

   function Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.down()");
   end Down;

   function Dig_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.digDown()");
   end Dig_Down;

   function Dig_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.digUp()");
   end Dig_Up;

   function Dig (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.dig()");
   end Dig;

   function Place (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.place()");
   end Place;

   function Place_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.placeDown()");
   end Place_Down;

   function Place_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.placeUp()");
   end Place_Up;

   procedure Select_Slot (T : Turtle; Slot : Turtle_Inventory_Slot) is
      Command : constant String  := "turtle.select(" & Slot'Image & ")";
      Result  : constant Boolean := Boolean_Function (T.Dispatcher, Command);
   begin
      if Result = False then
         raise Program_Error with Command & " returned False";
      end if;
   end Select_Slot;

   function Get_Item_Count
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Stack_Count
   is
      Command : constant String := "turtle.getItemCount(" & Slot'Image & ")";
      Result  : constant String := Raw_Function (T.Dispatcher, Command);
   begin
      return Stack_Count'Value (Result);
   end Get_Item_Count;

   function Get_Selected_Slot (T : Turtle) return Turtle_Inventory_Slot is
   begin
      return
        Turtle_Inventory_Slot'Value
          (Raw_Function (T.Dispatcher, "turtle.getSelectedSlot()"));
   end Get_Selected_Slot;

   function Get_Item_Detail (T : Turtle) return Item_Detail is
   begin
      return
        Parse_Item_Details
          (Raw_Function (T.Dispatcher, "turtle.getItemDetail()"));
   end Get_Item_Detail;

   function Get_Item_Detail
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Item_Detail
   is
      Command : constant String := "turtle.getItemDetail(" & Slot'Image & ")";
   begin
      return Parse_Item_Details (Raw_Function (T.Dispatcher, Command));
   end Get_Item_Detail;

   function Drop (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return
        Boolean_Function (T.Dispatcher, "turtle.drop(" & Amount'Image & ")");
   end Drop;

   function Detect (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.detect()");
   end Detect;

   function Detect_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.detectDown()");
   end Detect_Down;

   function Detect_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.detectUp()");
   end Detect_Up;

   procedure Forward (T : Turtle) is
      Result : constant Boolean := Forward (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Forward returned False";
      end if;
   end Forward;

   procedure Back (T : Turtle) is
      Result : constant Boolean := Back (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Back returned False";
      end if;
   end Back;

   procedure Up (T : Turtle) is
      Result : constant Boolean := Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Up returned False";
      end if;
   end Up;

   procedure Down (T : Turtle) is
      Result : constant Boolean := Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Down returned False";
      end if;
   end Down;

   procedure Dig_Down (T : Turtle) is
      Result : constant Boolean := Dig_Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig_Down returned False";
      end if;
   end Dig_Down;

   procedure Dig_Up (T : Turtle) is
      Result : constant Boolean := Dig_Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig_Up returned False";
      end if;
   end Dig_Up;

   procedure Dig (T : Turtle) is
      Result : constant Boolean := Dig (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig returned False";
      end if;
   end Dig;

   procedure Place (T : Turtle) is
      Result : constant Boolean := Place (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place returned False";
      end if;
   end Place;

   procedure Place_Down (T : Turtle) is
      Result : constant Boolean := Place_Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place_Down returned False";
      end if;
   end Place_Down;

   procedure Place_Up (T : Turtle) is
      Result : constant Boolean := Place_Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place_Up returned False";
      end if;
   end Place_Up;

   procedure Drop (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Drop (T, Amount);
   begin
      if Result = False then
         raise Program_Error
           with "Turtle.Drop(" & Amount'Image & ") returned False";
      end if;
   end Drop;

   procedure Maybe_Dig_Down (T : Turtle) is
      Result : constant Boolean := Dig_Down (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig_Down;

   procedure Maybe_Dig_Up (T : Turtle) is
      Result : constant Boolean := Dig_Up (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig_Up;

   procedure Maybe_Dig (T : Turtle) is
      Result : constant Boolean := Dig (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig;

   procedure Maybe_Place (T : Turtle) is
      Result : constant Boolean := Place (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place;

   procedure Maybe_Place_Down (T : Turtle) is
      Result : constant Boolean := Place_Down (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place_Down;

   procedure Maybe_Place_Up (T : Turtle) is
      Result : constant Boolean := Place_Up (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place_Up;

   --  private:

   function Parse_Item_Details (Table : String) return Item_Detail is
      Result : constant Item_Detail :=
        (Count => 0, Name => To_Unbounded_String (""));
      pragma Unreferenced (Table); -- TODO
   begin
      return Result;
   end Parse_Item_Details;

end Adabots_Turtles;
