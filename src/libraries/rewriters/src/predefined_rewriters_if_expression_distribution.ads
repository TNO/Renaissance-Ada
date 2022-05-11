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

package Predefined_Rewriters_If_Expression_Distribution is
   --  Rewriters for patterns involving the distribution of an operator over
   --  the alternative expressions of an if expression.

   --  TODO: how to generalize to other BinOp like +, *,
   --        and then, or else, ...?
   --  TODO: double check that this rewrite is correct
   --        (i.e. $S_Cond will always be executed before $S_Expr)
   --        for all possible operators (including x ** 0 == 1)

   function Accept_Independent (Match : Match_Pattern) return Boolean is
     (Are_Independent (Match, "$S_Cond", "$S_Expr"));

   Rewriter_Concat_Before_If_Expression_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $S_Expr & $S_True else $S_Expr & $S_False",
           Expr_Rule),
        Make_Pattern
          ("$S_Expr & (if $S_Cond then $S_True else $S_False)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Independent'Access));

   Rewriter_Concat_Before_If_Expression : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Concat_Before_If_Expression_Step);

   Rewriter_Concat_After_If_Expression_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $S_True & $S_Expr else $S_False & $S_Expr",
           Expr_Rule),
        Make_Pattern
          ("(if $S_Cond then $S_True else $S_False) & $S_Expr", Expr_Rule));

   Rewriter_Concat_After_If_Expression : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Concat_After_If_Expression_Step);

   Rewriter_Plus_Before_If_Expression_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $S_Expr + $S_True else $S_Expr + $S_False",
           Expr_Rule),
        Make_Pattern
          ("$S_Expr + (if $S_Cond then $S_True else $S_False)", Expr_Rule),
        Make_Match_Accepter_Function_Access (Accept_Independent'Access));

   Rewriter_Plus_Before_If_Expression : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Plus_Before_If_Expression_Step);

   Rewriter_Plus_After_If_Expression_Step :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $S_True + $S_Expr else $S_False + $S_Expr",
           Expr_Rule),
        Make_Pattern
          ("(if $S_Cond then $S_True else $S_False) + $S_Expr", Expr_Rule));

   Rewriter_Plus_After_If_Expression : aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_Plus_After_If_Expression_Step);

   Rewriter_If_Expression_Distribution : constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_Concat_Before_If_Expression &
        Rewriter_Concat_After_If_Expression &
        Rewriter_Plus_Before_If_Expression &
        Rewriter_Plus_After_If_Expression);
   --  Rewriter for patterns involving the distribution of an operator over
   --  the alternative expressions of an if expression.
   --
   --  The resulting code might still be simplified using
   --  * Simplify if expression
   --  * Minimal Parenthesis

end Predefined_Rewriters_If_Expression_Distribution;
