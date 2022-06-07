with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Langkit_Support.Text;        use Langkit_Support.Text;

package body Rewriters_Repeat is

   overriding function Rewrite
     (R_R       : Rewriter_Repeat; Unit : in out Analysis_Unit)
      return Boolean
   --  We should apply the rewrite as many times as possible
   --  Yet, we must report that the unit is changed when
   --  any actual change occurred.
   is
      Return_Value   : Boolean := False;
      Start_String   : Unbounded_String;
      Current_String : Unbounded_String :=
        To_Unbounded_String (Encode (Unit.Text, Unit.Get_Charset));
   begin
      while Start_String /= Current_String loop
         Start_String := Current_String;
         declare
            Changed : constant Boolean := R_R.Get_Rewriter.Rewrite (Unit);
         begin
            Return_Value := Return_Value or else Changed;
         end;
         Current_String :=
           To_Unbounded_String (Encode (Unit.Text, Unit.Get_Charset));
      end loop;
      return Return_Value;
   end Rewrite;

end Rewriters_Repeat;
