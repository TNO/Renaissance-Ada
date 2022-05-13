with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Sequence;          use Rewriters_Sequence;

with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;
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

   Rewrite_DeMorgan : constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewrite_De_Morgan_Not_And
        & Rewrite_De_Morgan_Not_Or
        & Rewrite_De_Morgan_Not_All_Range
        & Rewrite_De_Morgan_Not_All_Elements
        & Rewrite_De_Morgan_Not_Some_Range
        & Rewrite_De_Morgan_Not_Some_Elements
       );
   --  Rewriter for patterns that can be rewriting using DeMorgan's laws
   --  https://en.wikipedia.org/wiki/De_Morgan%27s_laws
   --
   --  The resulting code might still be simplified using
   --  * Not
   --  * Minimal Parenthesis

end Predefined_Rewriters_Boolean_Expression_De_Morgan;
