with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Operator_Definition_Equivalence is

   function Accept_Expr_No_Side_Effects (Match : Match_Pattern) return Boolean
   is
     (not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_Double : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr + $S_Expr", Expr_Rule),
        Make_Pattern ("2 * ($S_Expr)", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  Rewriter makes performance faster,
   --  especially when expression consumes a lot of time.
   --
   --  We don't rewrite in case of side effects:
   --  e.g. suppose the function f has a side effect.
   --  f(3) + f(3) triggers side effects twice
   --  while 2 * (f(3)) triggers side effects only once
   --  However how important is the side effect?
   --  Might be perfectly acceptable to log only once
   --  that f is entered, and f returns!
   --
   --  The resulting code might still be simplified using
   --  * Minimal Parenthesis

end Predefined_Rewriters_Operator_Definition_Equivalence;
