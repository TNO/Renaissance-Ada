with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Operator_Definition_Simplify is

   function Accept_Expr_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (not Has_Side_Effect (Match, "$S_Expr"));

   function Accept_Integer_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (Is_Integer_Expression (Match, "$S_Expr")
      and then not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_Definition_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr = $S_Expr", Expr_Rule),
        Make_Pattern ("true", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Definition_Different :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr /= $S_Expr", Expr_Rule),
        Make_Pattern ("false", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Definition_Minus : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr - $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Integer_No_Side_Effects'Access));
   --  TODO can it be correct for integers & float at the same time?

   Rewriter_Definition_Divide : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr / $S_Expr", Expr_Rule),
        Make_Pattern ("1", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Integer_No_Side_Effects'Access));
   --  TODO can it be correct for integers & float at the same time?

   Rewriter_Definition_Modulo : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr mod $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  TODO: can mod be overloaded or is it only defined for integer types?

   Rewriter_Definition_Remainder :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr rem $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  TODO: can rem be overloaded or is it only defined for integer types?

end Predefined_Rewriters_Operator_Definition_Simplify;
