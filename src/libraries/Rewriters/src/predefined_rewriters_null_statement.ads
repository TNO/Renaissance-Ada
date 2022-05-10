with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;

package Predefined_Rewriters_Null_Statement is

   Rewriter_Unnecessary_Null_Stmt :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Stmt; null;", Stmts_Rule),
        Make_Pattern ("$S_Stmt;", Stmt_Rule));
   --  TODO: do we also need the swapped version? i.e. "null; $S_Stmts;"

end Predefined_Rewriters_Null_Statement;
