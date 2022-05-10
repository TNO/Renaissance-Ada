with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Langkit_Support.Text;        use Langkit_Support.Text;

package body Rewriters_Repeat is

   overriding procedure Rewrite
     (R_R       : Rewriter_Repeat; Unit : in out Analysis_Unit)
   is
      Start_String   : Unbounded_String;
      Current_String : Unbounded_String :=
        To_Unbounded_String (Encode (Unit.Text, Unit.Get_Charset));
   begin
      while Start_String /= Current_String loop
         Start_String := Current_String;
         R_R.F_Rewriter.Rewrite (Unit);
         Current_String :=
           To_Unbounded_String (Encode (Unit.Text, Unit.Get_Charset));
      end loop;
   end Rewrite;

end Rewriters_Repeat;
