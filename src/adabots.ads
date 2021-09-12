with Adabots_Turtles;
with Adabots_Command_Computers;

package Adabots is

   type Turtle is new Adabots_Turtles.Turtle with null record;
   type Command_Computer is new Adabots_Command_Computers.Command_Computer with
   null record;

end Adabots;
