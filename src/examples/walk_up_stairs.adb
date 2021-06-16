with Adabots;

procedure Walk_Up_Stairs is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
begin
   loop
      while not Robot.Detect loop
         Robot.Forward;
      end loop;
      Robot.Up;
   end loop;
end Walk_Up_Stairs;
