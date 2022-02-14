with Ada.Text_IO;
with Ada.Exceptions;
with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Config;
with AWS.Config.Set;
with AAA.Strings;

package body Adabots_Lua_Dispatcher is

   function Create_Lua_Dispatcher (Port : Integer) return Lua_Dispatcher is
   begin
      return
        (Ada.Finalization.Limited_Controlled with
         Server => new Command_Server (Port));
   end Create_Lua_Dispatcher;

   type Server_Status is
     (Awaiting_Command, Sending_Command, Fetching_Return_Value,
      Returning_Result, Stopping);

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
         elsif AAA.Strings.Has_Prefix (URI, Return_Value_Prefix) then
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

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String
   is
      Returned_String : Unbounded_String;
   begin
      T.Server.Schedule_Command (Lua_Code);
      T.Server.Get_Result (Returned_String);
      declare
         String_Result : constant String := To_String (Returned_String);
      begin
         if AAA.Strings.Has_Prefix (String_Result, "error: ") then
            raise Program_Error with String_Result;
         end if;
         return String_Result;
      end;
   end Raw_Function;

   function Boolean_Function
     (T : Lua_Dispatcher; Lua_Code : String) return Boolean
   is
      Returned_String : constant String := T.Raw_Function (Lua_Code);
   begin
      if Returned_String = "true" then
         return True;
      elsif Returned_String = "false" then
         return False;
      end if;
      raise Program_Error with Returned_String;
   end Boolean_Function;

   procedure Raw_Procedure (T : Lua_Dispatcher; Lua_Code : String) is
      Result : String := Raw_Function (T, Lua_Code);
      pragma Unreferenced (Result);
   begin
      null;
   end Raw_Procedure;

   --  private:

   overriding procedure Finalize (T : in out Lua_Dispatcher) is
   begin
      Ada.Text_IO.Put_Line ("Shutting down...");
      T.Server.Shutdown;
      Ada.Text_IO.Put_Line ("Shutdown finished...");
   end Finalize;
end Adabots_Lua_Dispatcher;
