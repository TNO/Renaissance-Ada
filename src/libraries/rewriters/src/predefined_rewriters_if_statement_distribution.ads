with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;
with Rewriters_Repeat;            use Rewriters_Repeat;

package Predefined_Rewriters_If_Statement_Distribution is

   Rewriter_If_Stmt_Identical_Tail :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts_True; $S_Stmt; " &
           "else $M_Stmts_False; $S_Stmt; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond then $M_Stmts_True; " &
           "else $M_Stmts_False; end if; $S_Stmt;",
           Stmts_Rule));

   Rewriter_If_Stmt_Identical_Tails :
   aliased constant Rewriter_Repeat :=
     Make_Rewriter_Repeat (Rewriter_If_Stmt_Identical_Tail);

end Predefined_Rewriters_If_Statement_Distribution;
