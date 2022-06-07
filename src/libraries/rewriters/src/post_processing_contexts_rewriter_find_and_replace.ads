with Libadalang.Analysis;        use Libadalang.Analysis;
with Post_Processing_Contexts;   use Post_Processing_Contexts;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Rejuvenation;               use Rejuvenation;

package Post_Processing_Contexts_Rewriter_Find_And_Replace is

   type Post_Processing_Context_Rewriter_Find_And_Replace is
     new Post_Processing_Context with private;

   overriding function Get_Post_Processing_Context
     (P_P_C : Post_Processing_Context_Rewriter_Find_And_Replace;
      Unit  : Analysis_Unit) return Node_List.Vector;

   function Make_Post_Processing_Context_Rewriter_Find_And_Replace
     (R : Rewriter_Find_And_Replace)
      return Post_Processing_Context_Rewriter_Find_And_Replace;

private

   type Post_Processing_Context_Rewriter_Find_And_Replace is
     new Post_Processing_Context with
   record
      F_Rewriter : Rewriter_Find_And_Replace;
   end record;

   overriding function Get_Post_Processing_Context
     (P_P_C : Post_Processing_Context_Rewriter_Find_And_Replace;
      Unit  : Analysis_Unit) return Node_List.Vector is
     (P_P_C.F_Rewriter.Matching_Nodes (Unit));

   function Make_Post_Processing_Context_Rewriter_Find_And_Replace
     (R : Rewriter_Find_And_Replace)
      return Post_Processing_Context_Rewriter_Find_And_Replace is
     (F_Rewriter => R);

end Post_Processing_Contexts_Rewriter_Find_And_Replace;
