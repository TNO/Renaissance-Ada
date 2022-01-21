with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;

with Placeholder_Relations;         use Placeholder_Relations;
with Rewriters_Minimal_Parentheses; use Rewriters_Minimal_Parentheses;
with Rewriters_Find_And_Replace;    use Rewriters_Find_And_Replace;
with Rewriters_Sequence;            use Rewriters_Sequence;

--  Our experience so far is that you don't want to apply
--  any patch without reviewing.
--  Some rewrite opportunities are caused by
--  programmer errors, such as copy, paste, not modified errors.
--  By making the change, one loses the ability to spot the
--  real error, and make the necessary, correct change.

--  TODO: make smart rewrite - e.g. only RMP once
--  So rewriters is minimal required / rewriters can be removed
--  if they will be called anyway!

package Predefined_Rewriters is

------------------------------------------------------------------------------
   --  Expressions
------------------------------------------------------------------------------

   RMP : aliased constant Rewriter_Minimal_Parentheses :=
     Make_Rewriter_Minimal_Parentheses;

   Rewriter_Definition_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr = $S_Expr", Expr_Rule),
        Make_Pattern ("true", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Definition_Different :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr /= $S_Expr", Expr_Rule),
        Make_Pattern ("false", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Definition_Minus : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr - $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical
   --  TODO can it be correct for integers & float at the same time?

   Rewriter_Definition_Divide : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr / $S_Expr", Expr_Rule),
        Make_Pattern ("1", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical
   --  TODO can it be correct for integers & float at the same time?

   Rewriter_Definition_Modulo : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr mod $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Definition_Remainder :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr rem $S_Expr", Expr_Rule),
        Make_Pattern ("0", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Idempotence_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Idempotence_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else $S_Expr", Expr_Rule),
        Make_Pattern ("$S_Expr", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical

   Rewriter_Complementation_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr and then not $S_Expr", Expr_Rule),
        Make_Pattern ("false", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical
   --  TODO include variants with
   --            * swapped order of A and not A
   --            * parenthesis around not argument
   --              (only needed when additional parenthesis
   --               are allowed, e.g. for readability)

   Rewriter_Complementation_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr or else not $S_Expr", Expr_Rule),
        Make_Pattern ("true", Expr_Rule));
   --  TODO check for side effects in $S_Expr to ensure rewrite is identical
   --  TODO include variants with
   --            * swapped order of A and not A
   --            * parenthesis around not argument
   --              (only needed when additional parenthesis
   --               are allowed, e.g. for readability)

   Rewriter_Not_Not : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (not $S_Cond)", Expr_Rule),
        Make_Pattern ("$S_Cond", Expr_Rule));
   --  also known as "Double negation"

   Rewriter_Not_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left = $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left /= $S_Right)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Not_Different : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left /= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left = $S_Right)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   function Accept_Usage_Less_Equal (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, "<="));
   --  do not change the implementation of the "<=" operator

   Rewriter_Not_Greater_Than : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left > $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left <= $S_Right)", Expr_Rule),
        Accept_Usage_Less_Equal'Access,
        Rewriters => To_Vector (RMP'Access, 1));

   function Accept_Usage_Less_Than (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, "<"));
   --  do not change the implementation of the "<" operator

   Rewriter_Not_Greater_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left >= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left < $S_Right)", Expr_Rule),
        Accept_Usage_Less_Than'Access, Rewriters => To_Vector (RMP'Access, 1));

   function Accept_Usage_Greater_Equal
     (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, ">="));
   --  do not change the implementation of the ">=" operator

   Rewriter_Not_Less_Than : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left < $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left >= $S_Right)", Expr_Rule),
        Accept_Usage_Greater_Equal'Access,
        Rewriters => To_Vector (RMP'Access, 1));

   function Accept_Usage_Greater_Than (Match : Match_Pattern) return Boolean is
     (not Is_Within_Base_Subp_Body (Match, ">"));
   --  do not change the implementation of the ">" operator

   Rewriter_Not_Less_Equal : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Left <= $S_Right)", Expr_Rule),
        Make_Pattern ("($S_Left > $S_Right)", Expr_Rule),
        Accept_Usage_Greater_Than'Access,
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Not_In : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Var in $M_Values)", Expr_Rule),
        Make_Pattern ("($S_Var not in $M_Values)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Not_Not_In : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_Var not in $M_Values)", Expr_Rule),
        Make_Pattern ("($S_Var in $M_Values)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriters_Not : constant Rewriters_Sequence.Vector :=
     Rewriter_Not_Not'Access & Rewriter_Not_Equal'Access &
     Rewriter_Not_Different'Access & Rewriter_Not_Greater_Than'Access &
     Rewriter_Not_Greater_Equal'Access & Rewriter_Not_Less_Than'Access &
     Rewriter_Not_Less_Equal'Access & Rewriter_Not_In'Access &
     Rewriter_Not_Not_In'Access;

   --  TODO: check True is a Boolean
   --  TODO: do we also need the symmetric variant: true = $S_Var?
   Rewriter_Equal_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var = true", Expr_Rule),
        Make_Pattern ("$S_Var", Expr_Rule));

   Rewriter_Equal_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var = false", Expr_Rule),
        Make_Pattern ("not $S_Var", Expr_Rule));

   Rewriter_Different_True : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var /= true", Expr_Rule),
        Make_Pattern ("not $S_Var", Expr_Rule));

   Rewriter_Different_False : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var /= false", Expr_Rule),
        Make_Pattern ("$S_Var", Expr_Rule));

   Rewrite_De_Morgan_Not_And : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_A and then $S_B)", Expr_Rule),
        Make_Pattern ("(not ($S_A)) or else (not ($S_B))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewrite_De_Morgan_Not_Or : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not ($S_A or else $S_B)", Expr_Rule),
        Make_Pattern ("(not ($S_A)) and then (not ($S_B))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewrite_De_Morgan_Not_All_Range :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (for all $S_I in $S_Range => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for some $S_I in $S_Range => not ($S_Cond))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewrite_De_Morgan_Not_All_Elements :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("not (for all $S_E of $S_Elements => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for some $S_E of $S_Elements => not ($S_Cond))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewrite_De_Morgan_Not_Some_Range :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("not (for some $S_I in $S_Range => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for all $S_I in $S_Range => not ($S_Cond))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewrite_De_Morgan_Not_Some_Elements :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("not (for some $S_E of $S_Elements => $S_Cond)", Expr_Rule),
        Make_Pattern
          ("(for all $S_E of $S_Elements => not ($S_Cond))", Expr_Rule),
        Rewriters => Rewriters_Not);

   Rewriter_If_Different_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(if $S_A /= $S_B then $S_Val_True else $S_Val_False)", Expr_Rule),
        Make_Pattern
          ("(if $S_A = $S_B then $S_Val_False else $S_Val_True)", Expr_Rule));

   Rewriter_If_Not_Condition_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(if not $S_Cond then $S_Val_True else $S_Val_False)", Expr_Rule),
        Make_Pattern
          ("(if $S_Cond then $S_Val_False else $S_Val_True)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_If_Not_In_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(if $S_Expr not in $M_Values then $S_Val_True else $S_Val_False)",
           Expr_Rule),
        Make_Pattern
          ("(if $S_Expr in $M_Values then $S_Val_False else $S_Val_True)",
           Expr_Rule));

   Rewriter_Boolean_If_Condition_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("(if $S_Cond then true else false)", Expr_Rule),
        Make_Pattern ("($S_Cond)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Boolean_If_Not_Condition_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("(if $S_Cond then false else true)", Expr_Rule),
        Make_Pattern ("(not ($S_Cond))", Expr_Rule),
        Rewriters => Rewriters_Not & RMP'Access);

   function Accept_Constant_Expression
     (Match : Match_Pattern) return Boolean is
     (Is_Constant_Expression (Match, "$S_Expr"));

   --  TODO:
   --  replace constant check by the real check:
   --        $S_Cond doesn't effect $S_Expr AND
   --        $S_Expr doesn't effect $S_Cond
   --        such that the two placeholders can be reordered
   --        without changing the outcome
   --
   --        Note: effects include
   --       * output parameter of a function
   --         used in the other placeholder
   --       * side effect of a function (i.e. state change)
   --         used in the other placeholder
   Rewriter_Concat_Before_If_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(if $S_Cond then $S_Expr & $S_True else $S_Expr & $S_False)",
           Expr_Rule),
        Make_Pattern
          ("($S_Expr & (if $S_Cond then $S_True else $S_False))", Expr_Rule),
        Accept_Constant_Expression'Access,
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Concat_After_If_Expression :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(if $S_Cond then $S_True & $S_Expr else $S_False & $S_Expr)",
           Expr_Rule),
        Make_Pattern
          ("((if $S_Cond then $S_True else $S_False) & $S_Expr)", Expr_Rule),
        Rewriters => To_Vector (RMP'Access, 1));
--  TODO: how to generalize to other BinOp like +, *, and then, or else, ...?
--  TODO: double check that this rewrite is correct
   --        (i.e. $S_Cond will always be executed before $S_Expr)
   --        for all possible operators (including x ** 0 == 1)

   Rewriters_If_Expression : constant Rewriters_Sequence.Vector :=
     Rewriter_If_Different_Expression'Access &
     Rewriter_If_Not_Condition_Expression'Access &
     Rewriter_If_Not_In_Expression'Access &
     Rewriter_Boolean_If_Condition_Expression'Access &
     Rewriter_Boolean_If_Not_Condition_Expression'Access &
     Rewriter_Concat_Before_If_Expression'Access &
     Rewriter_Concat_After_If_Expression'Access;

   Rewriter_Case_Expression_Binary_With_Others :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("(case $S_Expr is when $M_Values => $S_Val_In, "
           & "when others => $S_Val_Out)",
           Expr_Rule),
        Make_Pattern
          ("(if ($S_Expr) in $M_Values then $S_Val_In else $S_Val_Out)",
           Expr_Rule),
        Rewriters => Rewriters_If_Expression & RMP'Access);

   --  TODO - comparable to If expressions
   --  once we have clear how to handle case statements
   --  with arbitrary number of alternatives
   --
   --  * Concat Before Case Expression
   --  * Concat After Case Expression

   --  TODO - comparable to Case Statement
   --  once we use Ada2022, that provides declaration expressions
   --
   --  * Case Single Expression

   --  TODO:
   --  replace constant check by the real check:
   --       Check for side effects:
   --       f(3) + f(3) has side effects (in f) twice
   --       while 2 * (f(3)) has side effects only once
   --
   --      However how important is the side effect?
   --      Might be perfectly acceptable to log only once
   --      that f is entered, and f returns!

   Rewriter_Double : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Expr + $S_Expr", Expr_Rule),
        Make_Pattern ("2 * ($S_Expr)", Expr_Rule),
        Accept_Constant_Expression'Access,
        Rewriters => To_Vector (RMP'Access, 1));

   Rewriter_Equals_To_In_Range : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var = $S_Val1 or else $S_Var = $S_Val2", Expr_Rule),
        Make_Pattern ("$S_Var in $S_Val1 | $S_Val2", Expr_Rule));

   Rewriter_Combine_In_Range_And_Equal :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var in $M_Vals or else $S_Var = $S_Val", Expr_Rule),
        Make_Pattern ("$S_Var in $M_Vals | $S_Val", Expr_Rule));

   Rewriter_Combine_In_Ranges : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var in $M_Vals_1 or else $S_Var in $M_Vals_2", Expr_Rule),
        Make_Pattern ("$S_Var in $M_Vals_1 | $M_Vals_2", Expr_Rule));

   Rewriter_Differents_To_Not_In_Range :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var /= $S_Val1 and then $S_Var /= $S_Val2", Expr_Rule),
        Make_Pattern ("$S_Var not in $S_Val1 | $S_Val2", Expr_Rule));

   Rewriter_Combine_Not_In_Range_And_Different :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var not in $M_Vals and then $S_Var /= $S_Val", Expr_Rule),
        Make_Pattern ("$S_Var not in $M_Vals | $S_Val", Expr_Rule));

   Rewriter_Combine_Not_In_Ranges :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var not in $M_Vals_1 and then $S_Var not in $M_Vals_2",
           Expr_Rule),
        Make_Pattern ("$S_Var not in $M_Vals_1 | $M_Vals_2", Expr_Rule));

------------------------------------------------------------------------------
   --  Statements
------------------------------------------------------------------------------

   Rewriter_Unnecessary_Null_Stmt :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Stmt; null;", Stmts_Rule),
        Make_Pattern ("$S_Stmt;", Stmt_Rule));
   --  TODO: do we also need the swapped version? i.e. "null; $S_Stmts;"

   Rewriter_If_True_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if true then $M_Stmts_True; else $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern ("$M_Stmts_True;", Stmts_Rule),
        Rewriters => To_Vector (Rewriter_Unnecessary_Null_Stmt'Access, 1));

   Rewriter_If_False_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if false then $M_Stmts_True; else $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern ("$M_Stmts_False;", Stmts_Rule),
        Rewriters => To_Vector (Rewriter_Unnecessary_Null_Stmt'Access, 1));

   --  Rewrite only when else branch is NOT empty
   Rewriter_If_Different_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_A /= $S_B then $M_Stmts_True; "
           & "else $S_Stmt_False; $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern ("", If_Stmt_Rule));

   --  Rewrite only when else branch is NOT empty
   Rewriter_If_Not_Condition_Stmt :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if not $S_Cond then $M_Stmts_True; "
           & "else $S_Stmt_False; $M_Stmts_False; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond then $S_Stmt_False; $M_Stmts_False; "
           & "else $M_Stmts_True; end if;",
           If_Stmt_Rule),
        Rewriters => To_Vector (RMP'Access, 1));

   --  Rewrite only when else branch is NOT empty
   Rewriter_If_Not_In_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Expr not in $M_Values " & "then $M_Stmts_True; " &
           "else $S_Stmt_False; $M_Stmts_False; " & "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Expr in $M_Values " &
           "then $S_Stmt_False; $M_Stmts_False; " & "else $M_Stmts_True; " &
           "end if;",
           If_Stmt_Rule));

   Rewriter_Use_Elsif : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond1 then $M_Stmts_True1; else " &
           "if $S_Cond2 then $M_Stmts_True2; else $M_Stmts_False2; end if; " &
           "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond1 then $M_Stmts_True1; " &
           "elsif $S_Cond2 then $M_Stmts_True2; else $M_Stmts_False2;" &
           "end if;",
           If_Stmt_Rule));

   Rewriter_Null_Then_Branch : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then null; else $S_Stmt; $M_Stmts; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if not ($S_Cond) then $S_Stmt; $M_Stmts; end if;", If_Stmt_Rule),
        Rewriters => Rewriters_Not);

   Rewriter_Null_Else_Branch : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts; else null; end if;", If_Stmt_Rule),
        Make_Pattern ("if $S_Cond then $M_Stmts; end if;", If_Stmt_Rule));

   Rewriter_If_Identical_Branches_Stmt :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts; else $M_Stmts; end if;", If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond then null; end if; "
           & "--  TODO: remove previous if statement "
                & "unless side effects are needed" &
           ASCII.LF & "$M_Stmts;",
           Stmts_Rule));

   Rewriter_If_Identical_Tails_Stmt :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then $M_Stmts_True; $S_Stmt; "
           & "else $M_Stmts_False; $S_Stmt; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("if $S_Cond then $M_Stmts_True; "
           & "else $M_Stmts_False; end if; $S_Stmt;",
           Stmts_Rule),
        Rewriters =>
          Rewriter_Null_Then_Branch'Access & Rewriter_Null_Else_Branch'Access);

   --  TODO: Add check to ensure that swapping $S_Cond and $M_Args_Before
   --        is allowed
   --
   --  Note that the order of evaluation of parameters is NOT specified in Ada
   --  see e.g. http://www.ada-auth.org/standards/12rat/html/Rat12-4-2.html
   --  hence also $M_Args_After might be effected and might have an effect!
   --
   --  Note that current implementation doesn't handle pattern as expected.
   --  We have no backtracking implemented yet.
   --  So, any match in the curent implementation will have
   --  an empty list for $M_Args_Before.
   Rewriter_If_Argument_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then " &
           "$S_Subp ($M_Args_Before, $M_Name => $S_Val_True, $M_Args_After);"
           & "else " &
           "$S_Subp ($M_Args_Before, $M_Name => $S_Val_False, $M_Args_After);"
           & "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("$S_Subp ($M_Args_Before," &
           "$M_Name => (if $S_Cond then $S_Val_True else $S_Val_False)," &
           "$M_Args_After);",
           Call_Stmt_Rule),
        Rewriters => Rewriters_If_Expression);

   Rewriter_If_Assignment_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then" & " $S_Var := $S_Val_True;" & "else" &
           " $S_Var := $S_Val_False;" & "end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("$S_Var := (if $S_Cond then $S_Val_True else $S_Val_False);",
           Stmt_Rule),
        Rewriters => Rewriters_If_Expression);

   Rewriter_If_Return_Stmt : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then return $S_Expr_True; "
           & "else return $S_Expr_False; end if;",
           If_Stmt_Rule),
        Make_Pattern
          ("return (if $S_Cond then $S_Expr_True else $S_Expr_False);",
           Return_Stmt_Rule),
        Rewriters => Rewriters_If_Expression);

   Rewriter_If_Return_Stmts : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("if $S_Cond then return $S_Expr_True; end if; "
           & "return $S_Expr_False;",
           Stmts_Rule),
        Make_Pattern
          ("return (if $S_Cond then $S_Expr_True else $S_Expr_False);",
           Return_Stmt_Rule),
        Rewriters => Rewriters_If_Expression);

   Rewriter_Case_Single : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is when $M_Values => $M_Stmts; end case;",
           Case_Stmt_Rule),
        Make_Pattern
          ("case $S_Expr is when $M_Values => null; end case; "
           & "--  TODO: remove previous case statement "
           & "unless side effects are needed" &
           ASCII.LF & "$M_Stmts;",
           Stmts_Rule));
--  In case of a case statement with a single alternative (single when branch),
--  the condition "($S_Expr) in $M_Values" is True:
--  Ada requires and the compiler enforces that
--  all possible values are included in the set of alternatives.
--  However, the evaluation of the expression can have a side effect.
--  So we can't leave it out.
--  Furthermore, we can't rewrite it to an if statement,
--  since $M_Values can be equal to the others keyword,
--  yet "($S_Expr) in others" is not a valid condition.

   Rewriter_Case_Binary_With_Others :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is when $M_Values => $M_Stmts_In; "
           & "when others => $M_Stmts_Out; end case;",
           Case_Stmt_Rule),
        Make_Pattern
          ("if ($S_Expr) in $M_Values then $M_Stmts_In; "
           & "else $M_Stmts_Out; end if;",
           If_Stmt_Rule),
        Rewriters =>
          Rewriter_Null_Else_Branch'Access & Rewriter_Null_Then_Branch'Access &
          RMP'Access);

   Rewriter_Case_Identical_Branches :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is " & "when $M_1_Vals => $M_Stmts;" &
           "when $M_2_Vals => $M_Stmts;" & "end case;",
           Case_Stmt_Rule),
        Make_Pattern
          ("case $S_Expr is when $M_1_Vals | $M_2_Vals => null; end case; "
           & "--  TODO: remove previous case statement "
           & "unless side effects are needed" &
           ASCII.LF & "$M_Stmts;",
           Stmts_Rule));
   --  TODO: How to make a concrete pattern matching
   --  an arbitrary number of alternatives?
   --  Or at least 2..N, where N is the largest number of alternatives
   --  in a case statement in the code base

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

   Rewriter_Return_Expression : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : constant $S_Type := $S_Expr; "
           & "begin return $S_Var; end;",
           Block_Stmt_Rule),
        Make_Pattern ("return $S_Expr;", Return_Stmt_Rule));

   function Accept_Variable (Match : Match_Pattern) return Boolean is
     (not Is_Referenced_In (Match, "$S_Var", "$S_Cond")
      and then not Is_Referenced_In (Match, "$S_Var", "$S_Val_True"));
   --  To ensure semantically correct rewrite, we have
   --  to check that
   --  1. $S_Var is NOT used in both $S_Cond and $S_Val_True
   --  2. TODO: swapping the order of execution of $S_Val_False and $S_Cond
   --     does not result in a different outcome
   --     (due to effects from one on the other)
   --  3. TODO: the execution of $S_Val_False doesn't effect
   --           the outcome of $S_Val_True
   --  4. TODO: the execution of $S_Val_False doesn't effect
   --           the outcome of $M_Stmts

   --  TODO: can we split this rewriter?
   --        Also add the constant keyword when appropriate!
   Rewriter_Declare_And_Overwrite :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : $S_Type := $S_Val_False; " &
           "begin if $S_Cond then $S_Var := $S_Val_True; end if; "
           & "$M_Stmts; end;",
           Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : $S_Type := " &
           "(if $S_Cond then $S_Val_True else $S_Val_False); " &
           "begin $M_Stmts; end;",
           Stmt_Rule),
        Accept_Variable'Access, Rewriters => Rewriters_If_Expression);

   function Accept_Single_Variable (Match : Match_Pattern) return Boolean is
     (not Is_Referenced_In (Match, "$S_I", "$S_Var"));
   --   prevent rewrite when $S_Var is an array access using $S_I

   Rewriter_For_All_Range_And_Then :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range "
           & "loop $S_Var := $S_Var and then $S_Cond; end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var and then (for all $S_I in $S_Range => $S_Cond);",
           Stmt_Rule),
        Accept_Single_Variable'Access);

   Rewriter_For_All_Elements_And_Then :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements loop " &
           "$S_Var := $S_Var and then $S_Cond;" & "end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var and then "
             & "(for all $S_E of $S_Elements => $S_Cond);",
           Stmt_Rule));

   Rewriter_For_Some_Range_Or_Else :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range "
           & "loop $S_Var := $S_Var or else $S_Cond; end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var or else (for some $S_I in $S_Range => $S_Cond);",
           Stmt_Rule),
        Accept_Single_Variable'Access);

   Rewriter_For_Some_Elements_Or_Else :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements loop " &
           "$S_Var := $S_Var or else $S_Cond;" & "end loop;",
           Loop_Stmt_Rule),
        Make_Pattern
          ("$S_Var := $S_Var or else "
             & "(for some $S_E of $S_Elements => $S_Cond);",
           Stmt_Rule));

   Rewriter_For_All_Range_Exit : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin "
           & "for $S_I in $S_Range "
           & "loop if $S_Cond then $S_Var := false; exit; end if; end loop; "
           & "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := "
           & "(for all $S_I in $S_Range => $S_Cond); "
           & "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_All_Elements_Exit :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := true; begin " &
           "for $S_E of $S_Elements loop " &
           "if $S_Cond then $S_Var := false; exit; end if; " & "end loop; " &
           "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := "
           & "(for all $S_E of $S_Elements => $S_Cond); "
           & "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_Some_Range_Exit : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("declare $S_Var : Boolean := false; begin "
           & "for $S_I in $S_Range "
           & "loop if $S_Cond then $S_Var := true; exit; end if; end loop; "
           & "$M_Stmts; end;",
           Block_Stmt_Rule),
        Make_Pattern
          ("declare $S_Var : Boolean := "
           & "(for some $S_I in $S_Range => $S_Cond); "
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
          ("declare $S_Var : Boolean := "
           & "(for some $S_E of $S_Elements => $S_Cond); "
           & "begin $M_Stmts; end;",
           Block_Stmt_Rule));

   Rewriter_For_All_Range_Return :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range "
           & "loop if $S_Cond then return false; end if; end loop; "
           & "return true;",
           Stmts_Rule),
        Make_Pattern
          ("return (for all $S_I in $S_Range => $S_Cond);", Return_Stmt_Rule));

   Rewriter_For_All_Elements_Return :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements "
           & "loop if $S_Cond then return false; end if; end loop; "
           & "return true;",
           Stmts_Rule),
        Make_Pattern
          ("return (for all $S_E of $S_Elements => $S_Cond);",
           Return_Stmt_Rule));

   Rewriter_For_Some_Range_Return :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_I in $S_Range "
           & "loop if $S_Cond then return true; end if; end loop; "
           & "return false;",
           Stmts_Rule),
        Make_Pattern
          ("return (for some $S_I in $S_Range => $S_Cond);",
           Return_Stmt_Rule));

   Rewriter_For_Some_Elements_Return :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("for $S_E of $S_Elements "
           & "loop if $S_Cond then return true; end if; end loop; "
           & "return false;",
           Stmts_Rule),
        Make_Pattern
          ("return (for some $S_E of $S_Elements => $S_Cond);",
           Return_Stmt_Rule));

------------------------------------------------------------------------------
   --  Declarations
------------------------------------------------------------------------------

   Rewriter_Declarations_Combine :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$M_X : $S_Type := $M_Expr;" & "$M_Y : $S_Type := $M_Expr;",
           Basic_Decls_Rule),
        Make_Pattern ("$M_X, $M_Y : $S_Type := $M_Expr;", Basic_Decl_Rule));

   Rewriter_For_Attribute_Use : aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : $S_Type := $M_Value with $M_Aspects;" &
           "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : $S_Type := $M_Value "
           & "with $M_Aspects, $S_Attribute => $S_Expr;",
           Basic_Decl_Rule));

   Rewriter_For_Attribute_Use_Aliased :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : aliased $S_Type := $M_Value with $M_Aspects;" &
           "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : aliased $S_Type := $M_Value "
           & "with $M_Aspects, $S_Attribute => $S_Expr;",
           Basic_Decl_Rule));

   Rewriter_For_Attribute_Use_Array :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : array ($M_Ranges) of $S_Type := "
           & "$M_Value with $M_Aspects;"
           & "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : array ($M_Ranges) of $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;",
           Basic_Decl_Rule));

   Rewriter_For_Attribute_Use_Pragma_Var :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : $S_Type := $M_Value with $M_Aspects;" &
           "pragma Warnings (Off, $S_Var);" &
           "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;" &
           "pragma Warnings (Off, $S_Var);",
           Basic_Decls_Rule));

   Rewriter_For_Attribute_Use_Pragma_All :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : $S_Type := $M_Value with $M_Aspects;" &
           "pragma Warnings (Off);" & "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;" &
           "pragma Warnings (Off);",
           Basic_Decls_Rule));

end Predefined_Rewriters;
