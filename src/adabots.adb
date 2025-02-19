with Ada.Environment_Variables;
with Adabots_Exceptions;
with Util.Strings;
with JSON.Parsers;
with JSON.Types;

package body Adabots is
   use Adabots_Lua_Dispatcher;

   -- private

   function Image (I : Integer) return String renames Util.Strings.Image;

   -- public

   function Create_Dispatcher (Bot_Name : String) return Adabots_Lua_Dispatcher.Lua_Dispatcher is
      Workspace_ID : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Environment_Variables.Value ("WORKSPACE_ID"));
      Unbounded_Bot_Name : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (Bot_Name);
   begin
      return Create_Lua_Dispatcher (Workspace_ID, Unbounded_Bot_Name);
   end;

   function Create_Turtle return Turtle is
   begin
      return Create_Turtle ("");
   end Create_Turtle;

   function Create_Turtle (Bot_Name : String) return Turtle is (Ada.Finalization.Limited_Controlled with Dispatcher => Create_Dispatcher (Bot_Name));

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

   function Trim_Left (Raw_Image : String) return String is
   begin
      return To_String (Trim(To_Unbounded_String (Raw_Image), Ada.Strings.Left));
   end Trim_Left;

   procedure Select_Slot (T : Turtle; Slot : Turtle_Inventory_Slot) is
      Command : constant String  := "turtle.select(" & Trim_Left(Slot'Image) & ")";
      Result  : constant Boolean := Boolean_Function (T.Dispatcher, Command);
   begin
      if Result = False
      then
         raise Program_Error with Command & " returned False";
      end if;
   end Select_Slot;

   function Get_Item_Count (T : Turtle; Slot : Turtle_Inventory_Slot) return Stack_Count is
      Command : constant String := "turtle.getItemCount(" & Trim_Left (Slot'Image) & ")";
      Result  : constant String := Raw_Function (T.Dispatcher, Command);
   begin
      return Stack_Count'Value (Result);
   end Get_Item_Count;

   function Get_Selected_Slot (T : Turtle) return Turtle_Inventory_Slot is
   begin
      return Turtle_Inventory_Slot'Value (Raw_Function (T.Dispatcher, "turtle.getSelectedSlot()"));
   end Get_Selected_Slot;

   function Get_Item_Detail (T : Turtle) return Item_Detail is
   begin
      return Parse_Item_Details (Raw_Function (T.Dispatcher, "turtle.getItemDetail()"));
   end Get_Item_Detail;

   function Get_Item_Detail (T : Turtle; Slot : Turtle_Inventory_Slot) return Item_Detail is
      Command : constant String := "turtle.getItemDetail(" & Trim_Left (Slot'Image) & ")";
   begin
      return Parse_Item_Details (Raw_Function (T.Dispatcher, Command));
   end Get_Item_Detail;

   function Get_Current_Tool (T : Turtle) return Tool_Info is
      Table : constant String := Raw_Function (T.Dispatcher, "turtle.getCurrentTool()");
   begin
      return Parse_Tool_Selection (Table);
   end Get_Current_Tool;

   function Drop (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.drop(" & Trim_Left (Amount'Image) & ")");
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

   function Detect_Left (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.detectLeft()");
   end Detect_Left;

   function Detect_Right (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.detectRight()");
   end Detect_Right;

   function Inspect (T : Turtle) return Adabots_Nodetypes.Node is
   begin
      return Adabots_Nodetypes.Convert (Inspect_String (T));
   exception
      when Adabots_Exceptions.Unknown_Nodetype =>
         return Adabots_Nodetypes.Unknown;
   end Inspect;
   function Inspect_Down (T : Turtle) return Adabots_Nodetypes.Node is
   begin
      return Adabots_Nodetypes.Convert (Inspect_Down_String (T));
   exception
      when Adabots_Exceptions.Unknown_Nodetype =>
         return Adabots_Nodetypes.Unknown;
   end Inspect_Down;
         
   function Inspect_Up (T : Turtle) return Adabots_Nodetypes.Node is
   begin
      return Adabots_Nodetypes.Convert (Inspect_Up_String (T));
   exception
      when Adabots_Exceptions.Unknown_Nodetype =>
         return Adabots_Nodetypes.Unknown;
   end Inspect_Up;

   function Inspect_String (T : Turtle) return String is (Raw_Function (T.Dispatcher, "turtle.inspect()"));
   function Inspect_Down_String (T : Turtle) return String is (Raw_Function (T.Dispatcher, "turtle.inspectDown()"));
   function Inspect_Up_String (T : Turtle) return String is (Raw_Function (T.Dispatcher, "turtle.inspectUp()"));

   function Suck (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.suck(" & Trim_Left (Amount'Image) & ")");
   end Suck;

   function Suck_Down (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.suckDown(" & Trim_Left (Amount'Image) & ")");
   end Suck_Down;

   function Suck_Up (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return Boolean_Function (T.Dispatcher, "turtle.suckUp(" & Trim_Left (Amount'Image) & ")");
   end Suck_Up;

   procedure Forward (T : Turtle) is
      Result : constant Boolean := Forward (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Forward returned False";
      end if;
   end Forward;

   procedure Back (T : Turtle) is
      Result : constant Boolean := Back (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Back returned False";
      end if;
   end Back;

   procedure Up (T : Turtle) is
      Result : constant Boolean := Up (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Up returned False";
      end if;
   end Up;

   procedure Down (T : Turtle) is
      Result : constant Boolean := Down (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Down returned False";
      end if;
   end Down;

   procedure Dig_Down (T : Turtle) is
      Result : constant Boolean := Dig_Down (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Dig_Down returned False";
      end if;
   end Dig_Down;

   procedure Dig_Up (T : Turtle) is
      Result : constant Boolean := Dig_Up (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Dig_Up returned False";
      end if;
   end Dig_Up;

   procedure Dig (T : Turtle) is
      Result : constant Boolean := Dig (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Dig returned False";
      end if;
   end Dig;

   procedure Place (T : Turtle) is
      Result : constant Boolean := Place (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Place returned False";
      end if;
   end Place;

   procedure Place_Down (T : Turtle) is
      Result : constant Boolean := Place_Down (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Place_Down returned False";
      end if;
   end Place_Down;

   procedure Place_Up (T : Turtle) is
      Result : constant Boolean := Place_Up (T);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Place_Up returned False";
      end if;
   end Place_Up;

   function Craft (T : Turtle; Amount : Positive_Stack_Count := 1) return Boolean is
      (Boolean_Function (T.Dispatcher, "turtle.craft(" & Trim_Left (Amount'Image) & ")"));

   procedure Craft (T : Turtle; Amount : Positive_Stack_Count := 1) is
      Result : constant Boolean := Craft (T, Amount);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Craft(" & Trim_Left (Amount'Image) & ") returned False";
      end if;
   end Craft;

   procedure Drop (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Drop (T, Amount);
   begin
      if Result = False
      then
         raise Program_Error with "Turtle.Drop(" & Trim_Left (Amount'Image) & ") returned False";
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

   procedure Suck (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Suck (T, Amount);
      pragma Unreferenced (Result);
   begin
      null;
   end Suck;

   procedure Suck_Down (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Suck_Down (T, Amount);
      pragma Unreferenced (Result);
   begin
      null;
   end Suck_Down;

   procedure Suck_Up (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Suck_Up (T, Amount);
      pragma Unreferenced (Result);
   begin
      null;
   end Suck_Up;

   --  private:

   package Types is new JSON.Types (Long_Integer, Long_Float);

   function Parse_Item_Details (Table : String) return Item_Detail is
      use Types;
      package Parsers is new JSON.Parsers (Types);
      Parser : Parsers.Parser := Parsers.Create (Table);
      Value : constant Types.JSON_Value := Parser.Parse;
      pragma Assert (Value.Kind = Object_Kind);
      pragma Assert (Value.Contains ("name"));
      Name_Value : constant Types.JSON_Value := Value ("name");
      pragma Assert (Name_Value.Kind = String_Kind);
      pragma Assert (Value.Contains ("count"));
      Count_Value : constant Types.JSON_Value := Value ("count");
      pragma Assert (Count_Value.Kind = Integer_Kind);
   begin
      return (Name => To_Unbounded_String (Name_Value.Image), Count => Stack_Count'Value(Count_Value.Image));
   end Parse_Item_Details;

   function Parse_Tool_Selection (Table : String) return Tool_Info is
      use Types;
      package Parsers is new JSON.Parsers (Types);
      Parser : Parsers.Parser := Parsers.Create (Table);
      Value : constant Types.JSON_Value := Parser.Parse;
      pragma Assert (Value.Kind = Object_Kind);
      pragma Assert (Value.Contains ("name"));
      Name_Value : constant Types.JSON_Value := Value ("name");
      pragma Assert (Name_Value.Kind = String_Kind);
      pragma Assert (Value.Contains ("remaining_uses"));
      Remaining_uses_Value : constant Types.JSON_Value := Value ("remaining_uses");
      pragma Assert (Remaining_uses_Value.Kind = Integer_Kind);
   begin
      return (Name => To_Unbounded_String (Name_Value.Image), Remaining_uses => Tool_Uses_Count'Value(Remaining_uses_Value.Image));
   end Parse_Tool_Selection;

   --  public:

   function Create_Command_Computer return Command_Computer is
   begin
      return Create_Command_Computer ("");
   end Create_Command_Computer;

   function Create_Command_Computer (Bot_Name : String) return Command_Computer is (Ada.Finalization.Limited_Controlled with Dispatcher => Create_Dispatcher (Bot_Name));

   function "+" (A, B : Relative_Location) return Relative_Location is
   begin
      return
         (X_Offset => A.X_Offset + B.X_Offset,
         Y_Offset => A.Y_Offset + B.Y_Offset,
         Z_Offset => A.Z_Offset + B.Z_Offset);
   end "+";

   function "-" (A, B : Relative_Location) return Relative_Location is
   begin
      return
         (X_Offset => A.X_Offset - B.X_Offset,
         Y_Offset => A.Y_Offset - B.Y_Offset,
         Z_Offset => A.Z_Offset - B.Z_Offset);
   end "-";

   function "+" (A, B : Absolute_Location) return Absolute_Location is
   begin
      return
         (X => A.X + B.X,
         Y => A.Y + B.Y,
         Z => A.Z + B.Z);
   end "+";

   function "+" (A : Absolute_Location; B : Relative_Location) return Absolute_Location is
   begin
      return
         (X => A.X + B.X_Offset,
         Y => A.Y + B.Y_Offset,
         Z => A.Z + B.Z_Offset);
   end "+";

   function Set_Block (C : Command_Computer; L : Relative_Location; B : Material) return Boolean is
      -- for example: commands.setblock('~20', '~', '~20', 'planks')
      Command : constant String :=
         "commands.setblock('~" & Image (L.X_Offset) & "', '~" &
         Image (L.Y_Offset) & "', '~" & Image (L.Z_Offset) & "', '" & Trim_Left (B'Image) &
         "')";
   begin
      return Boolean_Function (C.Dispatcher, Command);
   end Set_Block;

   procedure Maybe_Set_Block (C : Command_Computer; L : Relative_Location; B : Material) is
      Result : constant Boolean := Set_Block (C, L, B);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Set_Block;

   procedure Set_Cube
      (C : Command_Computer; First : Relative_Location; Last : Relative_Location; B : Material)
   is
   begin
      for Y in reverse First.Y_Offset .. Last.Y_Offset loop
         for X in First.X_Offset .. Last.X_Offset loop
            for Z in First.Z_Offset .. Last.Z_Offset loop
               Maybe_Set_Block
                  (C,
                  (X,
                  Y,
                  Z),
                  B);
            end loop;
         end loop;
      end loop;
   end Set_Cube;

   function Get_Block_Info (C : Command_Computer; L : Absolute_Location) return Material is
      Command : constant String :=
         "commands.getBlockInfo(" & Image (L.X) & ", " & Image (L.Y) & ", " &
         Image (L.Z) & ").name";
         Return_Value : constant String := Raw_Function (C.Dispatcher, Command);
         Prefix       : constant String := "minecraft:";
   begin
      return Material'Value (Util.Strings.Replace (Return_Value, Prefix, ""));
   end Get_Block_Info;
begin
   if not Ada.Environment_Variables.Exists("WORKSPACE_ID") then
      raise Program_Error with "No WORKSPACE_ID defined. Try `export WORKSPACE_ID=your_workspace_id` first.";
   end if;
end Adabots;
