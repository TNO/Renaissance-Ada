with Rejuvenation; use Rejuvenation;

package body Patchers is

   function Prepare_Unit (P : Patcher; Unit : in out Analysis_Unit)
                  return Boolean
   is
      Nodes : constant Node_List.Vector :=
        P.A_Context.Get_Post_Processing_Context (Unit);
   begin
      return Add_Marks_And_Pretty_Print_Sections (Unit, Nodes);
   end Prepare_Unit;

   procedure Rewrite (P : Patcher; Unit : in out Analysis_Unit) is
   begin
      if P.A_Rewriter.Rewrite (Unit) and then P.A_Post_Process /= null then
         declare
            Changed : constant Boolean := P.A_Post_Process.Rewrite (Unit);
         begin
            pragma Unreferenced (Changed);
         end;
      end if;
   end Rewrite;

end Patchers;
