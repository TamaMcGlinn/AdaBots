with Ada.Environment_Variables;
with Ada.Text_IO;
with Adabots_Exceptions;
with Ada.Exceptions;
with Util.Http.Clients.Curl;
with Util.Http.Clients;

package body Adabots_Lua_Dispatcher is

   function Create_Lua_Dispatcher
      (Workspace_ID : Ada.Strings.Unbounded.Unbounded_String; Bot_Name : Ada.Strings.Unbounded.Unbounded_String) return Lua_Dispatcher is
         ((Workspace_ID => Workspace_ID, Bot_Name => Bot_Name));

   function Begins_With (Source, Pattern : String) return Boolean is
   begin
      return Pattern'Length <= Source'Length 
        and then Source (Source'First .. Source'First + Pattern'Length - 1) = Pattern;
   end Begins_With;

   procedure Raise_Error (Error_String : String) is
      use Adabots_Exceptions;
   begin
      if Begins_With (Error_String, "timeout") then
         raise Instruction_Timeout with Error_String;
      end if;
      raise Unknown_Luanti_Error with Error_String;
   end Raise_Error;

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String is
      package Env renames Ada.Environment_Variables;
      -- GET request should look like this:
      -- {{instructionProxyBaseUrl}}/instruction-proxy/put?workspaceId={{workspaceId}}&botName={{botName}}&instruction=turtle.down()
      Default_Instruction_Proxy_Base_Url : constant String := "http://adabots.net";
      Instruction_Proxy_Base_Url : constant String :=
         (if Env.Exists ("INSTRUCTION_PROXY_BASE_URL") then
              Env.Value ("INSTRUCTION_PROXY_BASE_URL") else
              Default_Instruction_Proxy_Base_Url);
      Default_UserId : constant String := "undefined";
      User_Id : constant String :=
         (if Env.Exists ("CODER_WORKSPACE_OWNER_ID") then
              Env.Value ("CODER_WORKSPACE_OWNER_ID") else
              Default_UserId);
      Response : Util.Http.Clients.Response;
      Endpoint : constant String := Instruction_Proxy_Base_Url & "/instruction-proxy/put";
      Get_Args : constant String :=
        "workspaceId=" & Ada.Strings.Unbounded.To_String (T.Workspace_Id) &
        "&userId=" & User_Id &
        "&botName=" & Ada.Strings.Unbounded.To_String (T.Bot_Name) &
        "&instruction=" & Lua_Code;
      Request : constant String := Endpoint & "?" & Get_Args;
   begin
      Util.HTTP.Clients.Curl.Register;
      declare
         Http     : Util.Http.Clients.Client;
      begin
         Util.Http.Clients.Set_Timeout (Http, 2.0);
         Ada.Text_IO.Put_Line ("Scheduled " & Lua_Code);
         begin
            Http.Get (Request, Response);
         exception
         -- this happens when the timeout set above is lower than the server timeout
         -- if the server timeout is shorter, then it responds to us with an error
         -- which we parse in Raise_Error and also raise Instruction_Timeout
         when E : UTIL.HTTP.CLIENTS.CONNECTION_ERROR =>
            raise Adabots_Exceptions.Instruction_Timeout with Ada.Exceptions.Exception_Message(E);
         end;
         declare
            Response_Body : constant String := Response.Get_Body;
         begin
            if Begins_With (Response_Body, "error: ") then
               Raise_Error (Response_Body (8 .. Response_Body'Last));
            end if;
            return Response_Body;
         end;
      end;
   end Raw_Function;

   function Boolean_Function (T : Lua_Dispatcher; Lua_Code : String) return Boolean is
      Returned_String : constant String := Raw_Function (T, Lua_Code);
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

end Adabots_Lua_Dispatcher;
