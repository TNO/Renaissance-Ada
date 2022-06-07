with Libadalang.Analysis;             use Libadalang.Analysis;
with Libadalang.Common;               use Libadalang.Common;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;
with Placeholder_Relations;           use Placeholder_Relations;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;           use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;

package Predefined_Rewriters_Prefer_Quantified_Expressions is

   function Accept_Single_Variable (Match : Match_Pattern) return Boolean is
     (not Is_Referenced_In (Match, "$S_I", "$S_Var"));
   --   prevent rewrite when $S_Var is an array access using $S_I

   function Accept_Expr_No_Side_Effects
     (Match : Match_Pattern) return Boolean is
     (not Has_Side_Effect (Match, "$S_Expr"));

   Rewriter_For_All_Range_And_Then :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range " &
           "loop $S_Var := $S_Var and then $S_Cond; end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
        ("$S_Var := $S_Var and then (for all $S_I in $S_Range => $S_Cond);",
         Stmt_Rule),
        Make_Match_Accepter_Function_Access (Accept_Single_Variable'Access));

   Rewriter_For_All_Elements_And_Then :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements loop " &
           "$S_Var := $S_Var and then $S_Cond;" & "end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var and then " &
           "(for all $S_E of $S_Elements => $S_Cond);",
           Stmt_Rule));

   Rewriter_For_Some_Range_Or_Else :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range " &
           "loop $S_Var := $S_Var or else $S_Cond; end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
        ("$S_Var := $S_Var or else (for some $S_I in $S_Range => $S_Cond);",
         Stmt_Rule),
        Make_Match_Accepter_Function_Access (Accept_Single_Variable'Access));

   Rewriter_For_Some_Elements_Or_Else :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements loop " &
           "$S_Var := $S_Var or else $S_Cond;" & "end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var or else " &
           "(for some $S_E of $S_Elements => $S_Cond);",
           Stmt_Rule));

   Rewriter_For_All_Range_Exit :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin " &
           "for $S_I in $S_Range " &
           "loop if $S_Cond then $S_Var := false; exit; end if; end loop; "
           & "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
             "(for all $S_I in $S_Range => not ($S_Cond)); " &
             "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_All_Elements_Exit :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin " &
           "for $S_E of $S_Elements loop " &
           "if $S_Cond then $S_Var := false; exit; end if; " & "end loop; "
           & "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for all $S_E of $S_Elements => not ($S_Cond)); " &
           "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_Some_Range_Exit :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := false; begin " &
           "for $S_I in $S_Range " &
           "loop if $S_Cond then $S_Var := true; exit; end if; end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for some $S_I in $S_Range => $S_Cond); "
            & "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_Some_Elements_Exit :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := false; begin " &
           "for $S_E of $S_Elements loop " &
           "if $S_Cond then $S_Var := true; exit; end if; " & "end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for some $S_E of $S_Elements => $S_Cond); " &
           "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_All_Range_Return :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range " &
           "loop if $S_Cond then return false; end if; end loop; " &
           "return true;",
           Stmts_Rule),
        Make_Pattern
          ("return (for all $S_I in $S_Range => not ($S_Cond));",
           Return_Stmt_Rule));

   Rewriter_For_All_Elements_Return :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements " &
           "loop if $S_Cond then return false; end if; end loop; " &
           "return true;",
           Stmts_Rule),
        Make_Pattern
          ("return (for all $S_E of $S_Elements => not ($S_Cond));",
           Return_Stmt_Rule));

   Rewriter_For_Some_Range_Return :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range " &
           "loop if $S_Cond then return true; end if; end loop; " &
           "return false;",
           Stmts_Rule),
        Make_Pattern
          ("return (for some $S_I in $S_Range => $S_Cond);",
           Return_Stmt_Rule));

   Rewriter_For_Some_Elements_Return :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements " &
           "loop if $S_Cond then return true; end if; end loop; " &
           "return false;",
           Stmts_Rule),
        Make_Pattern
          ("return (for some $S_E of $S_Elements => $S_Cond);",
           Return_Stmt_Rule));

   Rewriter_For_All_Range_All :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin " &
           "for $S_I in $S_Range " &
           "loop if $S_Expr then $S_Var := false; end if; end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
             "(for all $S_I in $S_Range => not ($S_Expr)); " &
             "begin $M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_For_All_Elements_All :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin " &
           "for $S_E of $S_Elements loop " &
           "if $S_Expr then $S_Var := false; end if; " & "end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for all $S_E of $S_Elements => not ($S_Expr)); " &
           "begin $M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_For_Some_Range_All :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := false; begin " &
           "for $S_I in $S_Range " &
           "loop if $S_Expr then $S_Var := true; end if; end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for some $S_I in $S_Range => $S_Expr); "
           & "begin $M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_For_Some_Elements_All :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := false; begin " &
           "for $S_E of $S_Elements loop " &
           "if $S_Expr then $S_Var := true; end if; " & "end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := " &
           "(for some $S_E of $S_Elements => $S_Expr); " &
           "begin $M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Expr_No_Side_Effects'Access));

   Rewriter_Quantified_Expressions :
   aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_For_All_Range_And_Then &
          Rewriter_For_All_Elements_And_Then &
          Rewriter_For_Some_Range_Or_Else &
          Rewriter_For_Some_Elements_Or_Else &
          Rewriter_For_All_Range_Exit &
          Rewriter_For_All_Elements_Exit &
          Rewriter_For_Some_Range_Exit &
          Rewriter_For_Some_Elements_Exit &
          Rewriter_For_All_Range_Return &
          Rewriter_For_All_Elements_Return &
          Rewriter_For_Some_Range_Return &
          Rewriter_For_Some_Elements_Return &
          Rewriter_For_All_Range_All &
          Rewriter_For_All_Elements_All &
          Rewriter_For_Some_Range_All &
          Rewriter_For_Some_Elements_All
       );

   function Quantified_Expressions_Rewrite_Context
     (Unit : Analysis_Unit)
      return Node_List.Vector;

end Predefined_Rewriters_Prefer_Quantified_Expressions;
