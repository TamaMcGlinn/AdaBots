with Ada.Text_IO;
with Ada.Exceptions;
with AWS.Server;
with AWS.MIME;

package body Adabots is
   function Create_Turtle (Port : Integer := 7_112) return Turtle is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Server => new Command_Server (Port));
   end Create_Turtle;

   function Turn_Right (T : Turtle) return Boolean is
      Returned_String : Unbounded_String;
   begin
      T.Server.Schedule_Command ("turtle.turnRight()");
      T.Server.Get_Result (Returned_String);
      if Returned_String = "true" then
         return True;
      elsif Returned_String = "false" then
         return False;
      end if;
      raise Program_Error with To_String (Returned_String);
   end Turn_Right;

   overriding procedure Finalize (T : in out Turtle) is
   begin
      T.Server.Shutdown;
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

   task body Command_Server is
      HTTP_Server     : AWS.Server.HTTP;
      Next_Command    : Unbounded_String;
      Previous_Result : Unbounded_String;
      Status          : Server_Status := Awaiting_Command;

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
            declare
               Return_Value_Start : constant Natural :=
                 URI'First + Return_Value_Prefix'Length;
               Return_Value : constant String :=
                 URI (Return_Value_Start .. URI'Last);
            begin
               Push_Return_Value (Return_Value);
               return
                 AWS.Response.Build
                   (AWS.MIME.Text_Plain, To_Unbounded_String (""));
            end;
         end if;
         return
           AWS.Response.Build
             (AWS.MIME.Text_Plain, To_Unbounded_String ("error"));
      end Respond;
   begin
      AWS.Server.Start
        (HTTP_Server, "Adabots", Callback => Respond'Unrestricted_Access,
         Port                             => Port);
      Ada.Text_IO.Put_Line ("Command server started");

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
                  Ada.Text_IO.Put_Line ("Command server shutting down...");
                  AWS.Server.Shutdown (HTTP_Server);
                  Status := Stopping;
                  delay 60.0; -- TODO figure out why this is necessary;
         --  the next run needs to wait a whole minute before starting,
         --  otherwise you get this error when trying to start the HTTP_Server:
         --  raised AWS.NET.SOCKET_ERROR : Bind : [98] Address already in use
               end Shutdown;
            end select;
         elsif Status = Sending_Command then
            accept Fetch_Command (Command : out Unbounded_String) do
               Command := Next_Command;
               Ada.Text_IO.Put_Line ("Sent " & To_String (Command));
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

end Adabots;
