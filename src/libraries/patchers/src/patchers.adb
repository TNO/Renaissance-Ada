with Rejuvenation; use Rejuvenation;

package body Patchers is

   procedure Mark (P : Patcher; Unit : in out Analysis_Unit) is
      Nodes : constant Node_List.Vector :=
        P.A_Context.Get_Post_Processing_Context (Unit);
   begin
      Mark (Unit, Nodes);
   end Mark;

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
