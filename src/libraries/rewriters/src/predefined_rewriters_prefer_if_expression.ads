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

package Predefined_Rewriters_Prefer_If_Expression is
   --  TODO: Don't prefer If Expression when one of
   --  the placeholders $S_Val_True and $S_Val_False
   --  are already if expressions or case expressions

   function Accept_All_Independent (Match : Match_Pattern) return Boolean is
     (Are_Independent (Match, "$S_Cond", "$M_Args_Before")
      and then Are_Independent (Match, "$S_Cond", "$M_Args_After"));
   --  Note that the order of evaluation of parameters
   --  is NOT specified in Ada
   --  see e.g. http://www.ada-auth.org/standards/12rat/html/Rat12-4-2.html
   --  hence also $M_Args_After might be effected and might have an effect!

   Rewriter_If_Stmt_Subprogram_Single_Diffent_Argument :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then " &
           "$S_Subp ($M_Args_Before, $M_Name => $S_Val_True, $M_Args_After);" &
           "else " &
           "$S_Subp ($M_Args_Before, $M_Name => $S_Val_False, $M_Args_After);" &
           "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("$S_Subp ($M_Args_Before," &
           "$M_Name => (if $S_Cond then $S_Val_True else $S_Val_False)," &
           "$M_Args_After);",
           Call_Stmt_Rule),
        Make_Match_Accepter_Function_Access (Accept_All_Independent'Access));
   --  Note that our current implementation doesn't handle this pattern
   --  as one might expect, since we have not implemented multi matching.
   --  So, any match in the current implementation will have
   --  an empty list for $M_Args_Before.
   --  Multi matching has recently been added to the C++ version
   --  of the rejuvenation library
   --
   --  TODO: add check that nested if expressions &
   --        case expressions are prevented

   Rewriter_If_Stmt_Assignment : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then" & " $S_Var := $S_Val_True;" & "else" &
           " $S_Var := $S_Val_False;" & "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("$S_Var := (if $S_Cond then $S_Val_True else $S_Val_False);",
           Stmt_Rule));
   --  TODO: add check that nested if expressions &
   --        case expressions are prevented

   Rewriter_If_Stmt_Return : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then return $S_Expr_True; " &
           "else return $S_Expr_False; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("return (if $S_Cond then $S_Expr_True else $S_Expr_False);",
           Return_Stmt_Rule));
   --  TODO: add check that nested if expressions &
   --        case expressions are prevented

   Rewriter_If_Stmt_Return_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then return $S_Expr_True; end if; " &
           "return $S_Expr_False;",
           Stmts_Rule),
        Make_Pattern
          ("return (if $S_Cond then $S_Expr_True else $S_Expr_False);",
           Return_Stmt_Rule));
   --  TODO: add check that nested if expressions &
   --        case expressions are prevented

   Rewriter_If_Expression : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_If_Stmt_Subprogram_Single_Diffent_Argument &
        Rewriter_If_Stmt_Assignment & Rewriter_If_Stmt_Return &
        Rewriter_If_Stmt_Return_Stmt);

   function If_Expression_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector;

end Predefined_Rewriters_Prefer_If_Expression;
