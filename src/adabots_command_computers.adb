package body Adabots_Command_Computers is
   use Adabots_Lua_Dispatcher;

   --  public:

   function Create_Command_Computer return Command_Computer is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Dispatcher => Create_Lua_Dispatcher);
   end Create_Command_Computer;

   function Create_Command_Computer (Port : Integer) return Command_Computer is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Dispatcher => Create_Lua_Dispatcher (Port));
   end Create_Command_Computer;

   function Is_Valid_Location_Component (S : String) return Boolean is
   begin
      if S'Size < 1 then
         return False;
      end if;
      return True;
   end Is_Valid_Location_Component;

   function Create_Location
     (X : String; Y : String; Z : String) return Location
   is
   begin
      return
        (X_Size => X'Last, Y_Size => Y'Last, Z_Size => Z'Last,
         X      => (String_Size => X'Last, Value => X),
         Y      => (String_Size => Y'Last, Value => Y),
         Z      => (String_Size => Z'Last, Value => Z));
   end Create_Location;

   procedure Set_Block
     (C : Command_Computer; X : String; Y : String; Z : String; B : Material)
   is
   begin
      Set_Block (C, Create_Location (X, Y, Z), B);
   end Set_Block;

   procedure Set_Block (C : Command_Computer; L : Location; B : Material) is
   begin
      -- for example: commands.setblock('~20', '~', '~20', 'planks')
      Raw_Procedure
        (C.Dispatcher,
         "commands.setblock('" & L.X.Value & "', '" & L.Y.Value & "', '" &
         L.Z.Value & "', '" & B'Image & "')");
   end Set_Block;

end Adabots_Command_Computers;
