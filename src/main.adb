with Ada.Text_IO;
with Adabots;

procedure Main is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
begin
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Turn_Right));
end Main;
