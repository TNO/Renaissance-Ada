with Libadalang.Analysis;             use Libadalang.Analysis;
with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Not is

   Rewriter_Not_Not : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (not $S_Cond)", Expr_Rule),
        Make_Pattern ("$S_Cond", Expr_Rule));
   --  also known as "Double negation"

   Rewriter_Not_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left = $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left /= $S_Right)", Expr_Rule));

   Rewriter_Not_Different : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left /= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left = $S_Right)", Expr_Rule));

   Rewriter_Not_In : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Var in $M_Values)", Expr_Rule),
        Make_Pattern ("($S_Var not in $M_Values)", Expr_Rule));

   Rewriter_Not_Not_In : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Var not in $M_Values)", Expr_Rule),
        Make_Pattern ("($S_Var in $M_Values)", Expr_Rule));

   function Accept_Usage_Less_Equal (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, "<="));
   --  do not change the implementation of the "<=" operator

   Rewriter_Not_Greater_Than : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left > $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left <= $S_Right)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Usage_Less_Equal'Access));

   function Accept_Usage_Less_Than (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, "<"));
   --  do not change the implementation of the "<" operator

   Rewriter_Not_Greater_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left >= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left < $S_Right)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Usage_Less_Than'Access));

   function Accept_Usage_Greater_Equal
     (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, ">="));
   --  do not change the implementation of the ">=" operator

   Rewriter_Not_Less_Than : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left < $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left >= $S_Right)", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Usage_Greater_Equal'Access));

   function Accept_Usage_Greater_Than (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, ">"));
   --  do not change the implementation of the ">" operator

   Rewriter_Not_Less_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left <= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left > $S_Right)", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Usage_Greater_Than'Access));

   Rewriter_Not : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_Not_Not & Rewriter_Not_Equal & Rewriter_Not_Different &
        Rewriter_Not_In & Rewriter_Not_Not_In & Rewriter_Not_Greater_Than &
        Rewriter_Not_Greater_Equal & Rewriter_Not_Less_Than &
        Rewriter_Not_Less_Equal);
   --  Rewriter for patterns involving the `not` operator
   --  that can be simplified.
   --
   --  The resulting code might still be simplified using
   --  * Minimal Parenthesis

   function Matching_Not_Nodes (Unit : Analysis_Unit) return Node_List.Vector;
   --  Nodes within the unit that are marked as not nodes

end Predefined_Rewriters_Not;
