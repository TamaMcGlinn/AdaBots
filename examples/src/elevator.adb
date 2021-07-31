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
            declare
               Original_Detect : Boolean := Robot.Detect;
               Levels          : Natural := 0;
            begin
               loop
                  declare
                     Dont_Care : Boolean := Robot.Up;
                     pragma Unreferenced (Dont_Care);
                     New_Detect : constant Boolean := Robot.Detect;
                  begin
                     Levels := Levels + 1;
                     if New_Detect /= Original_Detect then
                        if Levels = 1 then
                           Original_Detect := New_Detect;
                        else
                           return;
                        end if;
                     end if;
                  end;
               end loop;
            end;
      end case;
   end;
end Elevator;
