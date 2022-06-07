with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_If_Statement_Simplify is
   --  TODO:
   --  Use Is_Negation function to capture
   --  all patterns in which the two branches can be swapped
   --  in a single pattern.

   Rewriter_If_True_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if true then $M_Stmts_True; else $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern ("$M_Stmts_True;", Stmts_Rule));
   --  Warning: by removing $M_Stmts_False; some with/use clauses
   --           might become obsolete and the compiler will
   --           produce warnings!

   Rewriter_If_False_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if false then $M_Stmts_True; else $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern ("$M_Stmts_False;", Stmts_Rule));
   --  Warning: by removing $M_Stmts_True; some with/use clauses
   --           might become obsolete and the compiler will
   --           produce warnings!

   function Accept_Cond_Is_Negation
     (Match : Match_Pattern) return Boolean is
     (Is_Negation_Expression  (Match, "$S_Cond"));

   Rewriter_If_Negation_Condition_Stmt :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts_True; " &
           "else $S_Stmt_False; $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if not ($S_Cond) then $S_Stmt_False; $M_Stmts_False; " &
           "else $M_Stmts_True; end if;",
           If_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Cond_Is_Negation'Access));
   --  Rewrite only when else branch is NOT empty
   --
   --  The resulting code might still be simplified using
   --  * Not
   --  * Minimal Parenthesis

   Rewriter_Null_Then_Branch : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then null; else $S_Stmt; $M_Stmts; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if not ($S_Cond) then $S_Stmt; $M_Stmts; end if;",
           If_Stmt_Rule));

   Rewriter_Null_Else_Branch : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts; else null; end if;", If_Stmt_Rule),
        Make_Pattern ("if $S_Cond then $M_Stmts; end if;", If_Stmt_Rule));

   function Accept_Expr_No_Side_Effects (Match : Match_Pattern) return Boolean
   is
     (not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_If_Identical_Branches_Stmt :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Expr then $M_Stmts; else $M_Stmts; end if;",
           If_Stmt_Rule),
        Make_Pattern ("$M_Stmts;", Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));
   --  We can't rewrite when $S_Expr has a side effect,
   --  because it would change the behaviour of the program.

   Rewriter_Use_Elsif : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond1 then $M_Stmts_True1; else " &
           "if $S_Cond2 then $M_Stmts_True2; else $M_Stmts_False2; end if; "
           &
           "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond1 then $M_Stmts_True1; " &
           "elsif $S_Cond2 then $M_Stmts_True2; else $M_Stmts_False2;" &
           "end if;",
           If_Stmt_Rule));

end Predefined_Rewriters_If_Statement_Simplify;
