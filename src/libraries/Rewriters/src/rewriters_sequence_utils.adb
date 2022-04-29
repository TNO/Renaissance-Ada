with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;                 use Ada.Text_IO;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;

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

end Rewriters_Sequence_Utils;
