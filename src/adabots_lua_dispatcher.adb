with Ada.Text_IO;
with Ada.Exceptions;
with Aws.Server;
with Aws.Response;
with Aws.Status;
with Aws.Mime;
with Aws.Config;
with Aws.Config.Set;
with Aaa.Strings;

package body Adabots_Lua_Dispatcher is

   function Create_Lua_Dispatcher (Port : Integer) return Lua_Dispatcher is
   begin
      return (Ada.Finalization.Limited_Controlled with Server => new Command_Server (Port));
   end Create_Lua_Dispatcher;

   type Server_Status is
     (Awaiting_Command,
      Sending_Command,
      Fetching_Return_Value,
      Returning_Result,
      Stopping);

   function Strip_Prefix (Source, Prefix : String) return String;
   function Strip_Prefix (Source, Prefix : String) return String is
   begin
      return Source (Source'First + Prefix'Length .. Source'Last);
   end Strip_Prefix;

   task body Command_Server is
      Http_Server     : Aws.Server.Http;
      Aws_Config      : Aws.Config.Object := Aws.Config.Default_Config;
      Next_Command    : Unbounded_String;
      Previous_Result : Unbounded_String;
      Status          : Server_Status     := Awaiting_Command;

      function Respond (Request : Aws.Status.Data) return Aws.Response.Data;
      function Respond (Request : Aws.Status.Data) return Aws.Response.Data is
         Uri                 : constant String := Aws.Status.Uri (Request);
         Return_Value_Prefix : constant String := "/return_value/";
         Command             : Unbounded_String;
      begin
         if Uri = "/"
         then
            Fetch_Command (Command);
            return Aws.Response.Build (Aws.Mime.Text_Plain, To_String (Command));
         elsif Aaa.Strings.Has_Prefix (Uri, Return_Value_Prefix)
         then
            Push_Return_Value (Strip_Prefix (Uri, Return_Value_Prefix));
            return Aws.Response.Build (Aws.Mime.Text_Plain, To_Unbounded_String (""));
         end if;
         return Aws.Response.Build (Aws.Mime.Text_Plain, To_Unbounded_String ("error"));
      end Respond;
   begin
      Aws.Config.Set.Reuse_Address (Aws_Config, True);
      Aws.Config.Set.Server_Port (Aws_Config, Port);
      Aws.Config.Set.Server_Name (Aws_Config, "Adabots");
      Aws.Server.Start (Http_Server, Callback => Respond'Unrestricted_Access, Config => Aws_Config);
      -- Ada.Text_IO.Put_Line ("Command server started");

      Command_Loop :
      loop
         if Status = Awaiting_Command
         then
            select
               accept Schedule_Command (Command : String) do
                  Ada.Text_Io.Put_Line ("Scheduled " & Command);
                  Next_Command := To_Unbounded_String (Command);
                  Status       := Sending_Command;
               end Schedule_Command;
            or
               accept Shutdown do
                  -- Ada.Text_IO.Put_Line ("Command server shutting down...");
                  Aws.Server.Shutdown (Http_Server);
                  Status := Stopping;
               end Shutdown;
            end select;
         elsif Status = Sending_Command
         then
            accept Fetch_Command (Command : out Unbounded_String) do
               Command := Next_Command;
               -- Ada.Text_IO.Put_Line ("Sent " & To_String (Command));
               Next_Command := To_Unbounded_String ("");
               Status       := Fetching_Return_Value;
            end Fetch_Command;
         elsif Status = Fetching_Return_Value
         then
            accept Push_Return_Value (Return_Value : String) do
               Previous_Result := To_Unbounded_String (Return_Value);
               Status          := Returning_Result;
            end Push_Return_Value;
         elsif Status = Returning_Result
         then
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
         Ada.Text_Io.Put_Line ("Unexpected error:");
         Ada.Text_Io.Put_Line (Ada.Exceptions.Exception_Information (Error));
         Ada.Text_Io.Skip_Line;
   end Command_Server;

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String is
      Returned_String : Unbounded_String;
   begin
      T.Server.Schedule_Command (Lua_Code);
      T.Server.Get_Result (Returned_String);
      declare
         String_Result : constant String := To_String (Returned_String);
      begin
         if Aaa.Strings.Has_Prefix (String_Result, "error: ")
         then
            raise Program_Error with String_Result;
         end if;
         return String_Result;
      end;
   end Raw_Function;

   function Boolean_Function (T : Lua_Dispatcher; Lua_Code : String) return Boolean is
      Returned_String : constant String := T.Raw_Function (Lua_Code);
   begin
      if Returned_String = "true"
      then
         return True;
      elsif Returned_String = "false"
      then
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
      Ada.Text_Io.Put_Line ("Shutting down...");
      T.Server.Shutdown;
      Ada.Text_Io.Put_Line ("Shutdown finished...");
   end Finalize;
end Adabots_Lua_Dispatcher;
