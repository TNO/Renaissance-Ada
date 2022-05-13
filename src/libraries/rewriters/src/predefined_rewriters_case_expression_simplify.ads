with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;

package Predefined_Rewriters_Case_Expression_Simplify is

   Rewriter_Case_Expression_Binary_With_Others :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("case $S_Expr is when $M_Values => $S_Val_In, " &
           "when others => $S_Val_Out",
           Expr_Rule),
        Make_Pattern
          ("if ($S_Expr) in $M_Values then $S_Val_In else $S_Val_Out",
           Expr_Rule));
   --  Rewriter for patterns involving the case expression
   --  that can be simplified.
   --
   --  The resulting code might still be simplified using
   --  * Simplify if expression

   --  TODO - comparable to If expressions
   --  once we have clear how to handle case statements
   --  with arbitrary number of alternatives
   --
   --  * Case Expression Distribution (concat, +, *, ...)

   --  TODO - comparable to Case Statement
   --  once we use Ada2022, that provides declaration expressions
   --
   --  * Case Single Expression

end Predefined_Rewriters_Case_Expression_Simplify;
