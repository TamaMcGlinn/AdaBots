with Adabots;

procedure Staircase_Down is
   Depth : constant Integer := 5;

   task type Single_Staircase is
      entry Start (Port : Integer);
   end Single_Staircase;

   task body Single_Staircase is
   begin
      accept Start (Port : Integer) do
         declare
            Robot : constant Adabots.Turtle := Adabots.Create_Turtle (Port);
         begin
            for D in 1 .. Depth loop
               Robot.Maybe_Dig;
               Robot.Forward;
               Robot.Maybe_Dig;
               Robot.Maybe_Dig_Up;
               Robot.Maybe_Dig_Down;
               Robot.Down;
            end loop;
         end;
      end Start;
   end Single_Staircase;

   One : Single_Staircase;
   Two : Single_Staircase;
begin
   One.Start (6_321);
   One.Start (6_322);
end Staircase_Down;
