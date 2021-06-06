with Adabots;

procedure Get_Stone is
   Robot         : constant Adabots.Turtle := Adabots.Create_Turtle;
   Depth         : constant Integer        := 20;
   Tunnel_Length : constant Integer        := 20;
begin

   --  go down
   for Current_Depth in 1 .. Depth loop
      Robot.Maybe_Dig_Down;
      Robot.Down;
   end loop;

   --  dig tunnel forward, big enough to walk in
   for Current_Length in 1 .. Tunnel_Length loop
      Robot.Maybe_Dig_Down;
      Robot.Maybe_Dig;
      Robot.Forward;
      Robot.Maybe_Dig_Up;
   end loop;

   --  walk back to shaft
   for Current_Length in 1 .. Tunnel_Length loop
      Robot.Back;
   end loop;

   --  come back up
   for Current_Depth in 1 .. Depth loop
      Robot.Up;
   end loop;

end Get_Stone;
