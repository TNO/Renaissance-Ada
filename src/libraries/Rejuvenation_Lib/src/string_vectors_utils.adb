with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body String_Vectors_Utils is

   function Join
     (V         : Vector;
      Separator : String  := "")
   return String
   is
   begin
      if V.Is_Empty then
         return "";
      end if;

      declare
         Return_Value : Unbounded_String :=
           To_Unbounded_String (V.First_Element);
      begin
         for Index in V.First_Index + 1 .. V.Last_Index loop
            Append (Return_Value, Separator & V.Element (Index));
         end loop;
         return To_String (Return_Value);
      end;
   end Join;

end String_Vectors_Utils;
