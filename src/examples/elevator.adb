with Ada.Text_IO;

with Adabots;

procedure Elevator is

   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;
   type Direction is (Up, Down);

begin

   Ada.Text_IO.Put_Line ("Up or Down?");
   Ada.Text_IO.Put ("> ");

   declare
      Input : constant String    := Ada.Text_IO.Get_Line;
      Dir   : constant Direction := Direction'Value (Input);
   begin
      case Dir is
         when Down =>
            loop
               exit when not Robot.Down; -- TODO or Robot.Detect;
            end loop;
         when Up =>
            loop
               declare
                  Dont_Care : Boolean := Robot.Up;
               begin
                  exit when Robot.Detect;
               end;
            end loop;
      end case;
   end;
end Elevator;
