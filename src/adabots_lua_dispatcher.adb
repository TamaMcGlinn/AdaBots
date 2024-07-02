with Ada.Environment_Variables;
with Ada.Text_IO;
with Util.Http.Clients.Curl;
with Util.Http.Clients;

package body Adabots_Lua_Dispatcher is

   function Create_Lua_Dispatcher
      (Workspace_ID : Ada.Strings.Unbounded.Unbounded_String; Bot_Name : Ada.Strings.Unbounded.Unbounded_String) return Lua_Dispatcher is
         ((Workspace_ID => Workspace_ID, Bot_Name => Bot_Name));

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String is
      package Env renames Ada.Environment_Variables;
      -- GET request should look like this:
      -- {{instructionProxyBaseUrl}}/instruction-proxy/put?workspaceId={{workspaceId}}&botName={{botName}}&instruction=turtle.down()
      Default_Instruction_Proxy_Base_Url : constant String := "http://adabots.net";
      Instruction_Proxy_Base_Url : constant String :=
         (if Env.Exists ("INSTRUCTION_PROXY_BASE_URL") then
              Env.Value ("INSTRUCTION_PROXY_BASE_URL") else
              Default_Instruction_Proxy_Base_Url);
      Response : Util.Http.Clients.Response;
      Endpoint : constant String := Instruction_Proxy_Base_Url & "/instruction-proxy/put";
      Get_Args : constant String := "workspaceId=" & Ada.Strings.Unbounded.To_String (T.Workspace_ID) &
         "&botName=" & Ada.Strings.Unbounded.To_String (T.Bot_Name) &
         "&instruction=" & Lua_Code;
      Request : constant String := Endpoint & "?" & Get_Args;
   begin
      Util.HTTP.Clients.Curl.Register;
      declare
         Http     : Util.Http.Clients.Client;
      begin
         Ada.Text_IO.Put_Line ("Scheduled " & Lua_Code);
         Http.Get (Request, Response);
         return Response.Get_Body;
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
