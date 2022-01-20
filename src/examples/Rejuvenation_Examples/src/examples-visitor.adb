with Ada.Text_IO;                 use Ada.Text_IO;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Examples.Visitor is

   procedure Demo_Visitor (Unit : Analysis_Unit);

   procedure Demo (File_Name : String) is
      Unit : constant Analysis_Unit := Analyze_File (File_Name);
   begin
      Put_Line ("=== Examples of Visitor =======");
      New_Line;
      Demo_Visitor (Unit);
      New_Line;
   end Demo;

   procedure Demo_Visitor (Unit : Analysis_Unit) is

      function Visit_Function (Node : Ada_Node'Class) return Visit_Status;
      function Visit_Function (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Call_Expr then
            Put_Line (Node.Image);
         end if;
         return Into; -- Always continue the traversal.
      end Visit_Function;

   begin
      Unit.Root.Traverse (Visit_Function'Access);
   end Demo_Visitor;

end Examples.Visitor;
