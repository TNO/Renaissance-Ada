with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Declaration_Simplify is

   function Accept_Expr_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (not Has_Side_Effect (Match, "$M_Expr"));

   Rewriter_Declarations_Combine :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$M_X : $S_Type := $M_Expr;" & "$M_Y : $S_Type := $M_Expr;",
           Basic_Decls_Rule),
        Make_Pattern ("$M_X, $M_Y : $S_Type := $M_Expr;", Basic_Decl_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

end Predefined_Rewriters_Declaration_Simplify;
