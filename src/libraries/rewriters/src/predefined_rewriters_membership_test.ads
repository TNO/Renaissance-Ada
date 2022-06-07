with Libadalang.Analysis;             use Libadalang.Analysis;
with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Repeat;                use Rewriters_Repeat;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Membership_Test is

   --  TODO: Should we ensure that $S_Var is really a variable?
   --  Currently return value checking code,
   --  like
   --  0 /= f (x) and then 0 /= g (y)
   --  is transformed to
   --  0 not in f (x) | g (y)
   --  although correct Ada, return value checking is not a membership test

   function Accept_Var_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (not Has_Side_Effect (Match, "$S_Var"));

   Rewriter_Equals_To_In_Range :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var = $S_Val1 or else $S_Var = $S_Val2",
                      Expr_Rule),
        Make_Pattern ("$S_Var in $S_Val1 | $S_Val2", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_In_Range_And_Equal_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var in $M_Vals or else $S_Var = $S_Val",
                      Expr_Rule),
        Make_Pattern ("$S_Var in $M_Vals | $S_Val", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_Equal_And_In_Range_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var = $S_Val or else $S_Var in $M_Vals",
                      Expr_Rule),
        Make_Pattern ("$S_Var in $S_Val | $M_Vals ", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_In_Ranges_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var in $M_Vals_1 or else $S_Var in $M_Vals_2", Expr_Rule),
        Make_Pattern ("$S_Var in $M_Vals_1 | $M_Vals_2", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Differents_To_Not_In_Range :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var /= $S_Val1 and then $S_Var /= $S_Val2", Expr_Rule),
        Make_Pattern ("$S_Var not in $S_Val1 | $S_Val2", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_Not_In_Range_And_Different_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var not in $M_Vals and then $S_Var /= $S_Val", Expr_Rule),
        Make_Pattern ("$S_Var not in $M_Vals | $S_Val", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_Different_And_Not_In_Range_Step :
       aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var /= $S_Val and then $S_Var not in $M_Vals", Expr_Rule),
        Make_Pattern ("$S_Var not in $S_Val | $M_Vals", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_Not_In_Ranges_Step :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var not in $M_Vals_1 and then $S_Var not in $M_Vals_2",
           Expr_Rule),
        Make_Pattern ("$S_Var not in $M_Vals_1 | $M_Vals_2", Expr_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_No_Side_Effects'Access));

   Rewriter_Combine_In_Step : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
             (Rewriter_Combine_In_Range_And_Equal_Step
              & Rewriter_Combine_Equal_And_In_Range_Step
              & Rewriter_Combine_In_Ranges_Step);

   Rewriter_Combine_In : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Combine_In_Step);

   Rewriter_Combine_Not_In_Step : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
            (Rewriter_Combine_Not_In_Range_And_Different_Step
             & Rewriter_Combine_Different_And_Not_In_Range_Step
             & Rewriter_Combine_Not_In_Ranges_Step);

   Rewriter_Combine_Not_In : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Combine_Not_In_Step);

   Rewriter_Membership_Test : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_Equals_To_In_Range
        & Rewriter_Combine_In
        & Rewriter_Differents_To_Not_In_Range
        & Rewriter_Combine_Not_In
        );
   --  Rewriter for patterns involving membership tests
   --  that can be simplified.

   function Membership_Rewrite_Context
     (Unit : Analysis_Unit)
      return Node_List.Vector;
   --  Nodes within the unit that are rewriten based on the membership rewrite
   --  Note: overestimation since repeating rewriters are involved

end Predefined_Rewriters_Membership_Test;
