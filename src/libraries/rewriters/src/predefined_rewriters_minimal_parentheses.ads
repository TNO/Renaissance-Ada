with Libadalang.Analysis;             use Libadalang.Analysis;
with Libadalang.Common;               use Libadalang.Common;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;

package Predefined_Rewriters_Minimal_Parentheses is
   --  Unfortunately gnatpp does not deal with parentheses.
   --  Change enhancement submitted to AdaCore
   --  https://gt3-prod-1.adacore.com/#/tickets/UA07-043
   --
   --  Poor man's solution to remove as many parentheses as possible

   function Are_Parentheses_Necessary (P_E : Paren_Expr) return Boolean;
   --  Are Parentheses necessary for this Paren_Expr?

   function Accept_No_Parentheses
     (Match : Match_Pattern) return Boolean is
     (not Are_Parentheses_Necessary
        (Match.Get_Nodes.First_Element.As_Paren_Expr));
   --  Accept matches that don't need parentheses.
   --  A match is of course a single node of kind Ada_Paren_Expr

   Rewriter_Minimal_Parentheses :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("($S_Expr)", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_No_Parentheses'Access));

end Predefined_Rewriters_Minimal_Parentheses;
