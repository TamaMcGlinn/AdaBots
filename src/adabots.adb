with Ada.Text_IO;
with Ada.Exceptions;
with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Config;
with AWS.Config.Set;

package body Adabots is

   --  public:

   function Create_Turtle return Turtle is
      Default_Port : constant := 7_112;
   begin
      Ada.Text_IO.Put_Line
        ("Which port should I output on? (default:" & Default_Port'Image &
         ")");
      Ada.Text_IO.Put ("> ");
      declare
         T : constant String  := Ada.Text_IO.Get_Line;
         P : constant Integer :=
           (if T = "" then Default_Port else Integer'Value (T));
      begin
         return Create_Turtle (P);
      end;
   end Create_Turtle;

   function Create_Turtle (Port : Integer) return Turtle is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Server => new Command_Server (Port));
   end Create_Turtle;

   procedure Turn_Right (T : Turtle) is
   begin
      Turtle_Procedure (T, "turtle.turnRight()");
   end Turn_Right;

   procedure Turn_Left (T : Turtle) is
   begin
      Turtle_Procedure (T, "turtle.turnLeft()");
   end Turn_Left;

   function Forward (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.forward()");
   end Forward;

   function Back (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.back()");
   end Back;

   function Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.up()");
   end Up;

   function Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.down()");
   end Down;

   function Dig_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.digDown()");
   end Dig_Down;

   function Dig_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.digUp()");
   end Dig_Up;

   function Dig (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.dig()");
   end Dig;

   function Place (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.place()");
   end Place;

   function Place_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.placeDown()");
   end Place_Down;

   function Place_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.placeUp()");
   end Place_Up;

   procedure Select_Slot (T : Turtle; Slot : Turtle_Inventory_Slot) is
      Command : constant String  := "turtle.select(" & Slot'Image & ")";
      Result  : constant Boolean := Boolean_Function (T, Command);
   begin
      if Result = False then
         raise Program_Error with Command & " returned False";
      end if;
   end Select_Slot;

   function Get_Item_Count
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Stack_Count
   is
      Command : constant String := "turtle.getItemCount(" & Slot'Image & ")";
      Result  : constant String := Raw_Function (T, Command);
   begin
      return Stack_Count'Value (Result);
   end Get_Item_Count;

   function Get_Selected_Slot (T : Turtle) return Turtle_Inventory_Slot is
   begin
      return
        Turtle_Inventory_Slot'Value
          (Raw_Function (T, "turtle.getSelectedSlot()"));
   end Get_Selected_Slot;

   function Get_Item_Detail (T : Turtle) return Item_Detail is
   begin
      return Parse_Item_Details (Raw_Function (T, "turtle.getItemDetail()"));
   end Get_Item_Detail;

   function Get_Item_Detail
     (T : Turtle; Slot : Turtle_Inventory_Slot) return Item_Detail
   is
      Command : constant String := "turtle.getItemDetail(" & Slot'Image & ")";
   begin
      return Parse_Item_Details (Raw_Function (T, Command));
   end Get_Item_Detail;

   function Drop (T : Turtle; Amount : Stack_Count := 64) return Boolean is
   begin
      return Boolean_Function (T, "turtle.drop(" & Amount'Image & ")");
   end Drop;

   function Detect (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.detect()");
   end Detect;

   function Detect_Down (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.detectDown()");
   end Detect_Down;

   function Detect_Up (T : Turtle) return Boolean is
   begin
      return Boolean_Function (T, "turtle.detectUp()");
   end Detect_Up;

   procedure Forward (T : Turtle) is
      Result : constant Boolean := Forward (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Forward returned False";
      end if;
   end Forward;

   procedure Back (T : Turtle) is
      Result : constant Boolean := Back (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Back returned False";
      end if;
   end Back;

   procedure Up (T : Turtle) is
      Result : constant Boolean := Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Up returned False";
      end if;
   end Up;

   procedure Down (T : Turtle) is
      Result : constant Boolean := Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Down returned False";
      end if;
   end Down;

   procedure Dig_Down (T : Turtle) is
      Result : constant Boolean := Dig_Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig_Down returned False";
      end if;
   end Dig_Down;

   procedure Dig_Up (T : Turtle) is
      Result : constant Boolean := Dig_Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig_Up returned False";
      end if;
   end Dig_Up;

   procedure Dig (T : Turtle) is
      Result : constant Boolean := Dig (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Dig returned False";
      end if;
   end Dig;

   procedure Place (T : Turtle) is
      Result : constant Boolean := Place (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place returned False";
      end if;
   end Place;

   procedure Place_Down (T : Turtle) is
      Result : constant Boolean := Place_Down (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place_Down returned False";
      end if;
   end Place_Down;

   procedure Place_Up (T : Turtle) is
      Result : constant Boolean := Place_Up (T);
   begin
      if Result = False then
         raise Program_Error with "Turtle.Place_Up returned False";
      end if;
   end Place_Up;

   procedure Drop (T : Turtle; Amount : Stack_Count := 64) is
      Result : constant Boolean := Drop (T, Amount);
   begin
      if Result = False then
         raise Program_Error
           with "Turtle.Drop(" & Amount'Image & ") returned False";
      end if;
   end Drop;

   procedure Maybe_Dig_Down (T : Turtle) is
      Result : constant Boolean := Dig_Down (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig_Down;

   procedure Maybe_Dig_Up (T : Turtle) is
      Result : constant Boolean := Dig_Up (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig_Up;

   procedure Maybe_Dig (T : Turtle) is
      Result : constant Boolean := Dig (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Dig;

   procedure Maybe_Place (T : Turtle) is
      Result : constant Boolean := Place (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place;

   procedure Maybe_Place_Down (T : Turtle) is
      Result : constant Boolean := Place_Down (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place_Down;

   procedure Maybe_Place_Up (T : Turtle) is
      Result : constant Boolean := Place_Up (T);
      pragma Unreferenced (Result);
   begin
      null;
   end Maybe_Place_Up;

   --  private:

   overriding procedure Finalize (T : in out Turtle) is
   begin
      Ada.Text_IO.Put_Line ("Shutting down...");
      T.Server.Shutdown;
      Ada.Text_IO.Put_Line ("Shutdown finished...");
   end Finalize;

   type Server_Status is
     (Awaiting_Command, Sending_Command, Fetching_Return_Value,
      Returning_Result, Stopping);

   function Starts_With (Source, Pattern : String) return Boolean;
   function Starts_With (Source, Pattern : String) return Boolean is
   begin
      return
        Pattern'Length <= Source'Length
        and then Source (Source'First .. Pattern'Length) = Pattern;
   end Starts_With;

   function Strip_Prefix (Source, Prefix : String) return String;
   function Strip_Prefix (Source, Prefix : String) return String is
   begin
      return Source (Source'First + Prefix'Length .. Source'Last);
   end Strip_Prefix;

   task body Command_Server is
      HTTP_Server     : AWS.Server.HTTP;
      AWS_Config      : AWS.Config.Object := AWS.Config.Default_Config;
      Next_Command    : Unbounded_String;
      Previous_Result : Unbounded_String;
      Status          : Server_Status     := Awaiting_Command;

      function Respond (Request : AWS.Status.Data) return AWS.Response.Data;
      function Respond (Request : AWS.Status.Data) return AWS.Response.Data is
         URI                 : constant String := AWS.Status.URI (Request);
         Return_Value_Prefix : constant String := "/return_value/";
         Command             : Unbounded_String;
      begin
         if URI = "/" then
            Fetch_Command (Command);
            return
              AWS.Response.Build (AWS.MIME.Text_Plain, To_String (Command));
         elsif Starts_With (URI, Return_Value_Prefix) then
            Push_Return_Value (Strip_Prefix (URI, Return_Value_Prefix));
            return
              AWS.Response.Build
                (AWS.MIME.Text_Plain, To_Unbounded_String (""));
         end if;
         return
           AWS.Response.Build
             (AWS.MIME.Text_Plain, To_Unbounded_String ("error"));
      end Respond;
   begin
      AWS.Config.Set.Reuse_Address (AWS_Config, True);
      AWS.Config.Set.Server_Port (AWS_Config, Port);
      AWS.Config.Set.Server_Name (AWS_Config, "Adabots");
      AWS.Server.Start
        (HTTP_Server, Callback => Respond'Unrestricted_Access,
         Config                => AWS_Config);
      -- Ada.Text_IO.Put_Line ("Command server started");

      Command_Loop :
      loop
         if Status = Awaiting_Command then
            select
               accept Schedule_Command (Command : String) do
                  Ada.Text_IO.Put_Line ("Scheduled " & Command);
                  Next_Command := To_Unbounded_String (Command);
                  Status       := Sending_Command;
               end Schedule_Command;
            or
               accept Shutdown do
                  -- Ada.Text_IO.Put_Line ("Command server shutting down...");
                  AWS.Server.Shutdown (HTTP_Server);
                  Status := Stopping;
               end Shutdown;
            end select;
         elsif Status = Sending_Command then
            accept Fetch_Command (Command : out Unbounded_String) do
               Command := Next_Command;
               -- Ada.Text_IO.Put_Line ("Sent " & To_String (Command));
               Next_Command := To_Unbounded_String ("");
               Status       := Fetching_Return_Value;
            end Fetch_Command;
         elsif Status = Fetching_Return_Value then
            accept Push_Return_Value (Return_Value : String) do
               Previous_Result := To_Unbounded_String (Return_Value);
               Status          := Returning_Result;
            end Push_Return_Value;
         elsif Status = Returning_Result then
            accept Get_Result (Result : out Unbounded_String) do
               Result          := Previous_Result;
               Previous_Result := To_Unbounded_String ("");
               Status          := Awaiting_Command;
            end Get_Result;
         end if;
         exit Command_Loop when Status = Stopping;
      end loop Command_Loop;

   exception
      when Error : others =>
         Ada.Text_IO.Put_Line ("Unexpected error:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
         Ada.Text_IO.Skip_Line;
   end Command_Server;

   function Raw_Function (T : Turtle; Lua_Code : String) return String is
      Returned_String : Unbounded_String;
   begin
      T.Server.Schedule_Command (Lua_Code);
      T.Server.Get_Result (Returned_String);
      declare
         String_Result : constant String := To_String (Returned_String);
      begin
         if Starts_With (String_Result, "error: ") then
            raise Program_Error with String_Result;
         end if;
         return String_Result;
      end;
   end Raw_Function;

   function Boolean_Function (T : Turtle; Lua_Code : String) return Boolean is
      Returned_String : constant String := T.Raw_Function (Lua_Code);
   begin
      if Returned_String = "true" then
         return True;
      elsif Returned_String = "false" then
         return False;
      end if;
      raise Program_Error with Returned_String;
   end Boolean_Function;

   function String_Function (T : Turtle; Lua_Code : String) return String is
      Returned_String    : constant String := T.Raw_Function (Lua_Code);
      String_Type_Prefix : constant String := "string: ";
   begin
      if Starts_With (Returned_String, String_Type_Prefix) then
         return Strip_Prefix (Returned_String, String_Type_Prefix);
      end if;
      raise Program_Error with Returned_String;
   end String_Function;

   procedure Turtle_Procedure (T : Turtle; Lua_Code : String) is
      Result : String := Raw_Function (T, Lua_Code);
      pragma Unreferenced (Result);
   begin
      null;
   end Turtle_Procedure;

   function Parse_Item_Details (Table : String) return Item_Detail is
      Result : constant Item_Detail :=
        (Count => 0, Name => To_Unbounded_String (""));
      pragma Unreferenced (Table); -- TODO
   begin
      return Result;
   end Parse_Item_Details;

end Adabots;
