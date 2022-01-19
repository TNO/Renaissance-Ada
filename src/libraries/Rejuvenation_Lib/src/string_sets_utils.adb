with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package body String_Sets_Utils is

   function From_Vector (V : Vector) return Set
   is
      Return_Value : Set;
   begin
      for E of V loop
         Return_Value.Include (E);
      end loop;
      return Return_Value;
   end From_Vector;

   function To_String (S : Set) return String
   is
      Return_Value : Unbounded_String;
      C : String_Sets.Cursor := S.First;
   begin
      while C /= String_Sets.No_Element loop
         Append (Return_Value, Element (C));
         C := Next (C);
         if C /= String_Sets.No_Element then
            Append (Return_Value, ", ");
         end if;
      end loop;
      return "{" & To_String (Return_Value) & "}";
   end To_String;

end String_Sets_Utils;
