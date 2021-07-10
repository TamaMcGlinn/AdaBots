with Adabots;

procedure Josephine is

   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;

   procedure Build_Pole_Upwards is
   begin
      for D in 1 .. 5 loop
         Robot.Up;
         Robot.Place_Down;
      end loop;
   end Build_Pole_Upwards;

   procedure Build_Pole_Backwards;
   procedure Build_Pole_Backwards is
   begin
      for D in 1 .. 5 loop
         Robot.Back;
         Robot.Place;
      end loop;
   end Build_Pole_Backwards;

   procedure Build_Pole_Downwards is
   begin
      for D in 1 .. 5 loop
         Robot.Down;
         Robot.Place;
      end loop;
   end Build_Pole_Downwards;

   procedure Build_Arch_Backward is
   begin

      Build_Pole_Upwards;
      Build_Pole_Backwards;
      Build_Pole_Downwards;

   end Build_Arch_Backward;

   procedure Go_There is
   begin
      Robot.Turn_Right;
      for D in 1 .. 5 loop
         Robot.Forward;
      end loop;
      Robot.Turn_Left;
      Robot.Forward;
   end Go_There;

   procedure Turn_Around is
   begin
      Robot.Turn_Left;
      Robot.Turn_Left;
   end Turn_Around;

   procedure Go_Up is
   begin
      for D in 1 .. 5 loop
         Robot.Up;
      end loop;
      Robot.Turn_Right;
      Robot.Forward;
      Robot.Turn_Right;
      Robot.Back;
   end Go_Up;

   procedure Build_Floor_Line is
   begin
      for D in 1 .. 5 loop
         Robot.Back;
         Robot.Place;
      end loop;
   end Build_Floor_Line;

   procedure Build_Floor is
   begin
      for D in 1 .. 3 loop
         Build_Floor_Line;

         if D = 2 then
            Robot.Turn_Left;
         else
            Robot.Turn_Right;
         end if;
         Robot.Back;
         if D = 2 then
            Robot.Turn_Left;
         else
            Robot.Turn_Right;
         end if;
         Robot.Back;
      end loop;

      Build_Floor_Line;

   end Build_Floor;

   procedure Go_To_Next_Floor is
   begin
      Robot.Up;
      Robot.Forward;
      Robot.Turn_Right;
      Robot.Forward;
      Robot.Turn_Right;
   end Go_To_Next_Floor;

   procedure Robot_Do_It is
   begin
      Build_Arch_Backward;
      Go_There;
      Turn_Around;
      Build_Arch_Backward;
      Go_Up;
      Build_Floor;
      Go_To_Next_Floor;
   end Robot_Do_It;

begin

   Robot_Do_It;
   --  for S in 1 .. 4 loop
   --     Robot.Select_Slot (S);
   --     Robot_Do_It;
   --  end loop;

end Josephine;
