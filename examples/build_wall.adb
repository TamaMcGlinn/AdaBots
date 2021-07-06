with Adabots;

procedure Build_Wall is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
begin

   --  Go to floor
   loop
      exit when Robot.Down = False;
   end loop;

   --  One back up
   Robot.Up;

   --  Turn around
   Robot.Turn_Left;
   Robot.Turn_Left;

   --  Build wall backwards
   for Wall_Index in 1 .. 3 loop
      Robot.Place_Down;
      Robot.Place_Up;
      Robot.Back;
      Robot.Place;
   end loop;

end Build_Wall;
