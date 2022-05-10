with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace;  use Rewriters_Find_And_Replace;

package Predefined_Rewriters_Representation_Clauses is
   --  TODO:
   --  Use $M_Pragmas and check for pragmas only to
   --  reduce the number of patterns
   --  (in particular remove the special cases
   --   Rewriter_For_Attribute_Use_Pragma_Var and
   --   Rewriter_For_Attribute_Use_Pragma_All)

   Rewriter_For_Attribute_Use :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : $S_Type := $M_Value with $M_Aspects;" &
           "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;",
           Basic_Decl_Rule));

   Rewriter_For_Attribute_Use_Aliased :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : aliased $S_Type := $M_Value with $M_Aspects;" &
           "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : aliased $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;",
           Basic_Decl_Rule));

   Rewriter_For_Attribute_Use_Array :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("$S_Var : array ($M_Ranges) of $S_Type := " &
           "$M_Value with $M_Aspects;" &
           "for $S_Var'$S_Attribute use $S_Expr;",
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
           "pragma Warnings (Off);"
           & "for $S_Var'$S_Attribute use $S_Expr;",
           Basic_Decls_Rule),
        Make_Pattern
          ("$S_Var : $S_Type := $M_Value " &
           "with $M_Aspects, $S_Attribute => $S_Expr;" &
           "pragma Warnings (Off);",
           Basic_Decls_Rule));

end Predefined_Rewriters_Representation_Clauses;
