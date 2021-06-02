with Ada.Text_IO;
with Adabots;

procedure Main is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
begin
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Turn_Right));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Turn_Left));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Forward));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Back));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Up));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Down));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Dig_Down));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Dig_Up));
   Ada.Text_IO.Put_Line (Boolean'Image (Robot.Dig));
end Main;
