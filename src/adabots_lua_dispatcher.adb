with Util.Strings;
with Ada.Text_IO;
with Util.Http.Clients.Curl;
with Util.Http.Clients;

package body Adabots_Lua_Dispatcher is

   function Image (I : Integer) return String renames Util.Strings.Image;

   function Create_Lua_Dispatcher (Workspace_ID : Integer) return Lua_Dispatcher is ((Workspace_ID => Workspace_ID));

   function Raw_Function (T : Lua_Dispatcher; Lua_Code : String) return String is
      URI      : constant String := "http://adabots.net/proxy";
      Response : Util.Http.Clients.Response;
      Data : constant String := "{""workspaceId"": """ & Image (T.Workspace_ID) & """, ""instruction"": """ & Lua_Code & """}";
   begin
      Util.HTTP.Clients.Curl.Register;
      declare
         Http     : Util.Http.Clients.Client;
      begin
         Ada.Text_IO.Put_Line ("Scheduled " & Lua_Code);
         Http.Add_Header ("content-type", "application/json");
         Http.Post (URI, Data, Response);
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
