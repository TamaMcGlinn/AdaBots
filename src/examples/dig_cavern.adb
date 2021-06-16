with Adabots;

procedure Dig_Cavern is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;

   procedure Dig_All is
   begin
      loop
         exit when not Robot.Dig;
      end loop;
      Robot.Maybe_Dig_Up;
      Robot.Maybe_Dig_Down;
   end Dig_All;

begin

   -- for Depth in 1 .. 15 loop
   --    Robot.Maybe_Dig_Down;
   --    Robot.Down;
   --    loop
   --       exit when not Robot.Dig;
   --    end loop;
   --    Robot.Forward;
   --    Robot.Maybe_Dig;
   --    Robot.Maybe_Dig_Up;
   -- end loop;

   -- Robot.Forward;

   for Width in 1 .. 8 loop
      for Length in 1 .. 15 loop
         Dig_All;
         Robot.Forward;
      end loop;
      if Width mod 2 = 0 then
         Robot.Turn_Left;
      else
         Robot.Turn_Right;
      end if;
      Dig_All;
      Robot.Forward;
      if Width mod 2 = 0 then
         Robot.Turn_Left;
      else
         Robot.Turn_Right;
      end if;
      Dig_All;
   end loop;

   -- come back up
   Robot.Up;
   loop
      exit when not Robot.Dig_Up;
      Robot.Up;
   end loop;
   for Heigh in 1 .. 3 loop
      Robot.Up;
   end loop;

   -- go to ground
   Robot.Forward;
   loop
      exit when Robot.Dig_Down;
      Robot.Down;
   end loop;

end Dig_Cavern;
