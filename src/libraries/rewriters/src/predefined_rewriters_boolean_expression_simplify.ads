with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Boolean_Expression_Simplify is

   function Accept_Expr_Boolean (Match : Match_Pattern) return Boolean is
     (Is_Boolean_Expression (Match, "$S_Expr"));

   function Accept_Expr_No_Side_Effects (Match : Match_Pattern) return Boolean
   is
     (not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_True_Or_Else : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("true or else $S_Expr", Expr_Rule),
        Make_Pattern ("true", Expr_Rule));

   Rewriter_False_Or_Else : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("false or else $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));

   Rewriter_True_And_Then : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("true and then $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));

   Rewriter_False_And_Then : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("false and then $S_Expr", Expr_Rule),
        Make_Pattern ("false", Expr_Rule));

   Rewriter_Or_Else_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else true", Expr_Rule),
        Make_Pattern ("true", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Or_Else_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else false", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));

   Rewriter_And_Then_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then true", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));

   Rewriter_And_Then_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then false", Expr_Rule),
        Make_Pattern ("false", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Idempotence_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Idempotence_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Complementation_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then not $S_Expr", Expr_Rule),
        Make_Pattern ("false", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  TODO include variants with
   --            * swapped order of A and not A
   --            * parenthesis around not argument
   --              (only needed when additional parenthesis
   --               are allowed, e.g. for readability)

   Rewriter_Complementation_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else not $S_Expr", Expr_Rule),
        Make_Pattern ("true", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  TODO include variants with
   --            * swapped order of A and not A
   --            * parenthesis around not argument
   --              (only needed when additional parenthesis
   --               are allowed, e.g. for readability)

   Rewriter_Equal_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr = true", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_Boolean'Access));
   --  TODO: do we also need the symmetric variant: true = $S_Expr?

   Rewriter_Equal_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr = false", Expr_Rule),
        Make_Pattern ("not $S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Expr_Boolean'Access));
   --  TODO: do we also need the symmetric variant: false = $S_Expr?

   Rewriter_Different_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr /= true", Expr_Rule),
        Make_Pattern ("not $S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Expr_Boolean'Access));
   --  TODO: do we also need the symmetric variant: true /= $S_Expr?

   Rewriter_Different_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr /= false", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Expr_Boolean'Access));
   --  TODO: do we also need the symmetric variant: false /= $S_Expr?

   --  TODO: simplification of distribution
   --  distribution or    (a or else b) and then (a or else c) =
   --                     (a or else (b and then c))
   --  distribution and   (a and then b) or else (a and then c) =
   --                     (a and then (b or else c))

   --  TODO: simplification of absorption law
   --  A or else (A and then B) = A
   --  A and then (A or else B) = A
   --  Variants needed?
   --  A or else (B and then A) = A  [ no side effects in B ]
   --  A and then (B or else A) = A  [ no side effects in B ]

end Predefined_Rewriters_Boolean_Expression_Simplify;
