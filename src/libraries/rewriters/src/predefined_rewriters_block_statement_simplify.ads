with Libadalang.Common;               use Libadalang.Common;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package Predefined_Rewriters_Block_Statement_Simplify is

   Rewriter_Return_Expression :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : constant $S_Type := $S_Expr; " &
           "begin return $S_Var; end;",
           Block_Stmt_Rule),
        Make_Pattern ("return $S_Expr;", Return_Stmt_Rule));

   function Accept_Variable (Match : Match_Pattern) return Boolean is
     (not Is_Referenced_In (Match, "$S_Var", "$S_Cond")
      and then not Is_Referenced_In (Match, "$S_Var", "$S_Val_True")
      and then Are_Independent (Match, "$S_Val_False", "$S_Cond")
      and then not Has_Effect_On (Match, "$S_Val_False", "$S_Val_True")
      and then not Has_Effect_On (Match, "$S_Val_False", "$M_Stmts"));
   --  To ensure semantically correct rewrite, we have
   --  to check that
   --  1. $S_Var is NOT used in both $S_Cond and $S_Val_True
   --  2. swapping the order of execution of $S_Val_False and $S_Cond
   --     does not result in a different outcome
   --     (due to effects from one on the other)
   --  3. the execution of $S_Val_False doesn't effect
   --     the outcome of $S_Val_True
   --  4. the execution of $S_Val_False doesn't effect
   --     the outcome of $M_Stmts

   --  TODO: can we split this rewriter?
   --        Also add the constant keyword when appropriate!
   Rewriter_Declare_And_Overwrite :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : $S_Type := $S_Val_False; " &
           "begin if $S_Cond then $S_Var := $S_Val_True; end if; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : $S_Type := " &
           "(if $S_Cond then $S_Val_True else $S_Val_False); " &
           "begin $M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Match_Accepter_Function_Access (Accept_Variable'Access));

end Predefined_Rewriters_Block_Statement_Simplify;
