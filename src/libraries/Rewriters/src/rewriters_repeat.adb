with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;
with Rewriters_Node_Rule_Utils;   use Rewriters_Node_Rule_Utils;

package body Rewriters_Repeat is

   overriding function Rewrite
     (R_R       : Rewriter_Repeat; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String
   is
      Rule : constant Grammar_Rule := Node_To_Rule (Node);

      Start_String   : Unbounded_String;
      Current_String : Unbounded_String :=
        To_Unbounded_String (Raw_Signature (Node));
   begin
      while Start_String /= Current_String loop
         declare
            Current_Unit : constant Analysis_Unit :=
              Analyze_Fragment (To_String (Current_String), Rule);
            Current_Node : constant Ada_Node := Current_Unit.Root;
         begin
            Start_String   := Current_String;
            Current_String :=
              To_Unbounded_String
                (R_R.F_Rewriter.Rewrite (Current_Node, Top_Level));
         end;
      end loop;
      return To_String (Current_String);
   end Rewrite;

   overriding function Rewrite_Context
     (R_R : Rewriter_Repeat; Node : Ada_Node'Class) return Ada_Node is
     (R_R.F_Rewriter.Rewrite_Context (Node));

end Rewriters_Repeat;
