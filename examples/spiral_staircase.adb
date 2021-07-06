with Adabots;

procedure Spiral_Staircase is

   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;

   procedure Place_Staircase (Staircase_Height : Integer) is
   begin
      for D in 1 .. Staircase_Height loop
         Robot.Up;
         Robot.Place_Down;
         Robot.Forward;
      end loop;
   end Place_Staircase;

begin

   Robot.Turn_Right;
   Robot.Forward;
   Robot.Turn_Left;

   Place_Staircase (6);
   Robot.Turn_Left;
   Place_Staircase (7);
   Robot.Turn_Left;

end Spiral_Staircase;
