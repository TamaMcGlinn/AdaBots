with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings;
with Adabots_Exceptions;

package body Adabots_Nodetypes is
   function Replace_First (Value : String; Target, Replacement : Character) return String is
      use all type Ada.Strings.Membership;
      F      : Positive;
      L      : Natural;
      Output : String := Value;
   begin
      Ada.Strings.Fixed.Find_Token (Value, Ada.Strings.Maps.To_Set (Target), Inside, F, L);
      if L > 0
      then
         Output (F) := Replacement;
      end if;
      return Output;
   end Replace_First;

   function Convert (Value : String) return Node is
      use Adabots_Exceptions;
   begin
      return Node'Value (Replace_First (Value, ':', '_'));
   exception
      when Constraint_Error =>
         raise Unknown_Nodetype with "Unknown node type: " & Value;
   end Convert;

   function Convert (Value : Node) return String is
   begin
      return Replace_First (Value'Image, '_', ':');
   end Convert;
end Adabots_Nodetypes;
