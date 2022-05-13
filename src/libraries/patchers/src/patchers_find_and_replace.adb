with Rejuvenation; use Rejuvenation;

package body Patchers_Find_And_Replace is

   overriding procedure Mark
     (P : Patcher_Find_And_Replace; Unit : in out Analysis_Unit)
   is
      Nodes : constant Node_List.Vector := P.F_Rewriter.Matching_Nodes (Unit);
   begin
      Mark (Unit, Nodes);
   end Mark;

   overriding procedure Rewrite
     (P : Patcher_Find_And_Replace; Unit : in out Analysis_Unit)
   is
   begin
      if P.F_Rewriter.Rewrite (Unit) and then P.F_Post_Process /= null then
         declare
            Changed : constant Boolean := P.F_Post_Process.Rewrite (Unit);
         begin
            pragma Unreferenced (Changed);
         end;
      end if;
   end Rewrite;

end Patchers_Find_And_Replace;
