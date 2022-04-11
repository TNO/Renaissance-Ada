with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;                 use Ada.Text_IO;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;
with Rewriters_Context_Utils;     use Rewriters_Context_Utils;

package body Rewriters_Sequence_Utils is

   function Rewrite
     (RS        : Rewriters_Sequence.Vector; Node : Ada_Node'Class;
      Top_Level : Boolean      := False;
      Rule      : Grammar_Rule := Default_Grammar_Rule) return String
   is
      Current_Node : Ada_Node      := Node.As_Ada_Node;
      Unit         : Analysis_Unit := No_Analysis_Unit;
      --  don't release context before Current_Node is used
   begin
      for R of RS loop
         declare
            Str : constant String := R.Rewrite (Current_Node, Top_Level);
         begin
            Unit         := Analyze_Fragment (Str, Rule);
            Current_Node := Unit.Root;
         end;
      end loop;
      return Raw_Signature (Current_Node);
   exception
      when Error : others =>
         Put_Line
           ("Error in Rewrite - Rewriters_Sequence.Vector " &
            Exception_Message (Error));
         raise;
   end Rewrite;

   function Rewrite_Context
     (RS : Rewriters_Sequence.Vector; Node : Ada_Node'Class) return Ada_Node
   is
      Return_Value : Ada_Node := Node.As_Ada_Node;
   begin
      for R of RS loop
         Return_Value :=
           Combine_Contexts (Return_Value, R.Rewrite_Context (Node));
      end loop;
      return Return_Value;
   end Rewrite_Context;

end Rewriters_Sequence_Utils;
