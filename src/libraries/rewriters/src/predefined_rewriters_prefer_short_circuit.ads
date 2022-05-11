with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Prefer_Short_Circuit is
   --  Use short circuit form of logical operators
   --  Rewrite only occurs when behaviour is identical

   function Accept_Right_Boolean_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (Is_Boolean_Expression (Match, "$S_Right")
      and then not Has_Side_Effect (Match, "$S_Right"));

   Rewriter_And_Then : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Left and $S_Right", Expr_Rule),
        Make_Pattern ("$S_Left and then $S_Right", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Right_Boolean_No_Side_Effects'Access));
   --  Use short circuit form of logical `and` operator

   Rewriter_Or_Else : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Left or $S_Right", Expr_Rule),
        Make_Pattern ("$S_Left or else $S_Right", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Right_Boolean_No_Side_Effects'Access));
   --  Use short circuit form of logical `or` operator

end Predefined_Rewriters_Prefer_Short_Circuit;
