with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;
with Rewriters_Repeat;            use Rewriters_Repeat;
with Rewriters_Sequence;          use Rewriters_Sequence;
with Rewriters_Vectors;           use Rewriters_Vectors;

package Predefined_Rewriters_Boolean_Expression_De_Morgan is
   --  Rewriters for patterns that can be rewriting using De Morgan's laws
   --  https://en.wikipedia.org/wiki/De_Morgan%27s_laws

   Rewrite_De_Morgan_Not_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_A and then $S_B)", Expr_Rule),
        Make_Pattern ("(not ($S_A)) or else (not ($S_B))", Expr_Rule));

   Rewrite_De_Morgan_Not_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_A or else $S_B)", Expr_Rule),
        Make_Pattern ("(not ($S_A)) and then (not ($S_B))", Expr_Rule));

   Rewrite_De_Morgan_Not_All_Range :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (for all $S_I in $S_Range => $S_Cond)",
                      Expr_Rule),
        Make_Pattern
          ("(for some $S_I in $S_Range => not ($S_Cond))", Expr_Rule));

   Rewrite_De_Morgan_Not_All_Elements :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("not (for all $S_E of $S_Elements => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for some $S_E of $S_Elements => not ($S_Cond))", Expr_Rule));

   Rewrite_De_Morgan_Not_Some_Range :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (for some $S_I in $S_Range => $S_Cond)",
                      Expr_Rule),
        Make_Pattern
          ("(for all $S_I in $S_Range => not ($S_Cond))", Expr_Rule));

   Rewrite_De_Morgan_Not_Some_Elements :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("not (for some $S_E of $S_Elements => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for all $S_E of $S_Elements => not ($S_Cond))", Expr_Rule));

   Rewrite_De_Morgan_Step : constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewrite_De_Morgan_Not_And
        & Rewrite_De_Morgan_Not_Or
        & Rewrite_De_Morgan_Not_All_Range
        & Rewrite_De_Morgan_Not_All_Elements
        & Rewrite_De_Morgan_Not_Some_Range
        & Rewrite_De_Morgan_Not_Some_Elements
       );

   Rewrite_De_Morgan : constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewrite_De_Morgan_Step);
   --  Rewriter for patterns that can be rewriting using DeMorgan's laws
   --  https://en.wikipedia.org/wiki/De_Morgan%27s_laws
   --
   --  The resulting code might still be simplified using
   --  * Not
   --  * Minimal Parenthesis

   function De_Morgan_Rewrite_Context
     (Unit : Analysis_Unit)
      return Node_List.Vector;

end Predefined_Rewriters_Boolean_Expression_De_Morgan;
