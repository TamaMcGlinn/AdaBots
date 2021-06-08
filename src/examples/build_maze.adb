with Adabots;

procedure Build_Maze is
   Robot : constant Adabots.Turtle := Adabots.Create_Turtle;

   Maze_Width  : constant Integer := 3;
   Maze_Length : constant Integer := 3;
   type Cell is (Filled, Empty);

   type Maze is array (0 .. Maze_Width - 1, 0 .. Maze_Length - 1) of Cell;
   M : constant Maze := (others => (others => Filled));

   procedure Build_Cell (C : Cell) is
   begin
      if C = Filled then
         Robot.Place_Up;
         Robot.Place_Down;
      end if;
      Robot.Back;
      if C = Filled then
         Robot.Place;
      end if;
   end Build_Cell;

begin
   Robot.Up;
   Robot.Turn_Left;
   Robot.Turn_Left;
   for W in M'Range (1) loop
      for L in M'Range (2) loop
         Build_Cell (M (W, L));
      end loop;
      if W mod 2 = 0 then
         Robot.Turn_Left;
         Robot.Back;
         Robot.Turn_Left;
         Robot.Back;
      else
         Robot.Turn_Right;
         Robot.Back;
         Robot.Turn_Right;
         Robot.Back;
      end if;
   end loop;
   Robot.Down;
end Build_Maze;
