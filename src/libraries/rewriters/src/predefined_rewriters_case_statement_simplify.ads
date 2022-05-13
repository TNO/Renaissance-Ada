with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Case_Statement_Simplify is

   function Accept_Expr_No_Side_Effects (Match : Match_Pattern) return Boolean
   is
     (not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_Case_Single : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is when $M_Values => $M_Stmts; end case;",
           Case_Stmt_Rule),
        Make_Pattern ("$M_Stmts;", Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  In case of a case statement with a single alternative
   --  (single when branch),
   --  the condition "($S_Expr) in $M_Values" is True:
   --  Ada requires and the compiler enforces that
   --  all possible values are included in the set of alternatives.
   --  When the evaluation of the expression has a side effect,
   --  we can't leave it out.

   Rewriter_Case_Identical_Branches :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is " & "when $M_1_Vals => $M_Stmts;" &
               "when $M_2_Vals => $M_Stmts;" & "end case;",
           Case_Stmt_Rule),
        Make_Pattern ("$M_Stmts;", Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  TODO: How to make a concrete pattern matching
   --  an arbitrary number of alternatives?
   --  Or at least 2..N, where N is the largest number of alternatives
   --  in a case statement in the code base

   Rewriter_Case_Binary_With_Others :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is when $M_Values => $M_Stmts_In; " &
               "when others => $M_Stmts_Out; end case;",
           Case_Stmt_Rule),
        Make_Pattern
          ("if ($S_Expr) in $M_Values then $M_Stmts_In; " &
               "else $M_Stmts_Out; end if;",
           If_Stmt_Rule));
   --  The resulting code might still be simplified using
   --  * Simplify If statement
   --  * Minimal Parenthesis

   --  TODO - comparable to If statements
   --  once we have clear how to handle case statements
   --  with arbitrary number of alternatives
   --
   --  * Case Argument
   --  * Case Assignment
   --
   --  And also
   --  * Case combine alternatives
   --    i.e. change "when $M_X => $M_Stmts; when $M_Y => $M_Stmts;"
   --         to     "when $M_X | $M_Y => $M_Stmts;"

end Predefined_Rewriters_Case_Statement_Simplify;
