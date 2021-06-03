with Ada.Text_IO;
with Adabots;

procedure Main is
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
end Main;
