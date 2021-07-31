-- with Adabots;
-- with Ada.Sequential_IO;
-- with Ada.Text_IO;
-- with Interfaces;

procedure Lovelace is
-- use type Interfaces.Unsigned_8;
-- Image_Width  : constant := 50;
-- Image_Height : constant := 50;
-- type Color is (White, Light_Gray, Gray, Black);
--
-- -- Blocks       : array (1 .. Image_Width, 1 .. Image_Height) of Boolean;
--
-- package Byte_IO is new Ada.Sequential_IO
--   (Element_Type => Interfaces.Unsigned_8);
--
-- Picture : Byte_IO.File_Type;
-- Data    : array (1 .. 4) of Interfaces.Unsigned_8;
--
-- procedure Skip (Bytes : Integer) is
--    Junk : Interfaces.Unsigned_8;
-- begin
--    for Skip in 1 .. Bytes loop
--       Byte_IO.Read (Picture, Junk);
--    end loop;
-- end Skip;

begin
   null;
   -- Skip (18);
   -- for X in 1 .. Image_Width loop
   --    for Y in 1 .. Image_Height loop
   --       for Data_Index in Data'Range loop
   --          Byte_IO.Read (Picture, Data (Data_Index));
   --       end loop;
   --       if Data /= 0 then
   --          Ada.Text_IO.Put ('X');
   --       else
   --          Ada.Text_IO.Put (' ');
   --       end if;
   --    end loop;
   --    Ada.Text_IO.New_Line;
   -- end loop;
end Lovelace;
