with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_If_Expression_Simplify is
   --  TODO:
   --  Use Is_Negation function to capture
   --  all patterns in which the two alternatives can be swapped
   --  in a single pattern.

   Rewriter_If_True_Expression : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if true then $S_Val_True else $S_Val_False", Expr_Rule),
        Make_Pattern ("$S_Val_True", Expr_Rule));

   Rewriter_If_False_Expression : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if false then $S_Val_True else $S_Val_False", Expr_Rule),
        Make_Pattern ("$S_Val_False", Expr_Rule));

   function Accept_Cond_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (not Has_Side_Effect (Match, "$S_Cond"));

   Rewriter_If_Identical_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then $S_Expr else $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Cond_No_Side_Effects'Access));

   function Accept_Cond_Is_Negation
     (Match : Match_Pattern) return Boolean is
     (Is_Negation_Expression  (Match, "$S_Cond"));

   Rewriter_If_Negation_Condition_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $S_Val_True else $S_Val_False", Expr_Rule),
        Make_Pattern
          ("if not ($S_Cond) then $S_Val_False else $S_Val_True", Expr_Rule),
       Make_Match_Accepter_Function_Access
          (Accept_Cond_Is_Negation'Access));

   Rewriter_Boolean_If_Condition_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then true else false", Expr_Rule),
        Make_Pattern ("$S_Cond", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   Rewriter_Boolean_If_Not_Condition_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then false else true", Expr_Rule),
        Make_Pattern ("not ($S_Cond)", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   Rewriter_Boolean_If_Then_True_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then true else $S_Y", Expr_Rule),
        Make_Pattern ("$S_Cond or else $S_Y", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   Rewriter_Boolean_If_Then_False_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then false else $S_Y", Expr_Rule),
        Make_Pattern ("not ($S_Cond) and then $S_Y", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   Rewriter_Boolean_If_Else_True_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then $S_Y else true", Expr_Rule),
        Make_Pattern ("not ($S_Cond) or else $S_Y", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   Rewriter_Boolean_If_Else_False_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_Cond then $S_Y else false", Expr_Rule),
        Make_Pattern ("$S_Cond and then $S_Y", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Is_Boolean_Expression'Access));

   function Accept_Extreme (Match : Match_Pattern) return Boolean is
     (Is_Integer_Expression (Match, "$S_X")
      and then not Has_Side_Effect (Match, "$S_X")
      and then Is_Integer_Expression (Match, "$S_Y")
      and then not Has_Side_Effect (Match, "$S_Y"));

   Rewriter_Integer_Max_Greater_Than :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X > $S_Y then $S_X else $S_Y", Expr_Rule),
        Make_Pattern ("Integer'Max ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Max_Greater_Equal :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X >= $S_Y then $S_X else $S_Y", Expr_Rule),
        Make_Pattern ("Integer'Max ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Max_Less_Than :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X < $S_Y then $S_Y else $S_X", Expr_Rule),
        Make_Pattern ("Integer'Max ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Max_Less_Equal :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X <= $S_Y then $S_Y else $S_X", Expr_Rule),
        Make_Pattern ("Integer'Max ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Min_Greater_Than :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X > $S_Y then $S_Y else $S_X", Expr_Rule),
        Make_Pattern ("Integer'Min ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Min_Greater_Equal :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X >= $S_Y then $S_Y else $S_X", Expr_Rule),
        Make_Pattern ("Integer'Min ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Min_Less_Than :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X < $S_Y then $S_X else $S_Y", Expr_Rule),
        Make_Pattern ("Integer'Min ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_Integer_Min_Less_Equal :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("if $S_X <= $S_Y then $S_X else $S_Y", Expr_Rule),
        Make_Pattern ("Integer'Min ($S_X, $S_Y)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Extreme'Access));

   Rewriter_If_Expression_Simplify : constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_If_True_Expression & Rewriter_If_False_Expression &
          Rewriter_If_Identical_Expression &
          Rewriter_If_Negation_Condition_Expression &
          Rewriter_Boolean_If_Condition_Expression &
          Rewriter_Boolean_If_Not_Condition_Expression &
          Rewriter_Boolean_If_Then_True_Expression &
          Rewriter_Boolean_If_Then_False_Expression &
          Rewriter_Boolean_If_Else_True_Expression &
          Rewriter_Boolean_If_Else_False_Expression &
          Rewriter_Integer_Max_Greater_Than &
          Rewriter_Integer_Max_Greater_Equal & Rewriter_Integer_Max_Less_Than &
          Rewriter_Integer_Max_Less_Equal & Rewriter_Integer_Min_Greater_Than &
          Rewriter_Integer_Min_Greater_Equal & Rewriter_Integer_Min_Less_Than &
          Rewriter_Integer_Min_Less_Equal);
   --  Rewriter for patterns involving the if expression
   --  that can be simplified.
   --
   --  The resulting code might still be simplified using
   --  * Not
   --  * Minimal Parenthesis

end Predefined_Rewriters_If_Expression_Simplify;
