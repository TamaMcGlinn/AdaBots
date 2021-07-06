with Adabots;

procedure Remove_Wall is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
begin

   --  Destroy wall
   for Wall_Index in 1 .. 3 loop
      Robot.Dig;
      Robot.Forward;
      Robot.Dig_Up;
      Robot.Dig_Down;
   end loop;

   --  Turn around
   Robot.Turn_Left;
   Robot.Turn_Left;

   --  Go up a couple of units
   for Height in 1 .. 3 loop
      Robot.Up;
   end loop;

end Remove_Wall;
