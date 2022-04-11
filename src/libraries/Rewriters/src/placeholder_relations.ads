with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;

package Placeholder_Relations is

   function Get_Expression_Type
     (Match : Match_Pattern; Expression : String) return Base_Type_Decl with
      Pre => Match.Has_Single (Expression);
      --  Return Base_Decl_type of Expression in Match

   function Is_Referenced_In
     (Match : Match_Pattern; Definition, Context : String) return Boolean;

   function Is_Constant_Expression
     (Match : Match_Pattern; Expression : String) return Boolean;
   --  Is Expression in Match a constant expression?
   --  Note: 3, 3+4, "abc" & "def",  and 4*(5+7) are all constant expressions
   --  TODO: Can we use libadalang's
   --      function P_Is_Static_Expr
   --        (Node : Expr'Class;
   --         Imprecise_Fallback : Boolean := False) return Boolean;
   --         --  Return whether this expression is static according to the
   --         --  ARM definition of static. See RM 4.9.
   --  for our purpose?

   function Has_Side_Effect
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean;
      --  Has Execution of Expression a side effect?
      --  Side effects include:
      --     variables are changed, write to file, write to screen, ...

   function Has_Effect_On
     (Match : Match_Pattern; Placeholder_A, Placeholder_B : String)
      return Boolean;
   --  Does place_holder effect place_holder B?
   --  Effects include
   --       * output parameter of a function
   --         used in the other placeholder
   --       * side effect of a function (i.e. state change)
   --         used in the other placeholder

   function Are_Independent
     (Match : Match_Pattern; Placeholder_1, Placeholder_2 : String)
      return Boolean;
   --  Are the placeholders independent?
   --  In other words, can the order of execution of these placeholders
   --  be changed without changing the behaviour of the program?

   function Is_Within_Base_Subp_Body
     (Match : Match_Pattern; Subp_Name : String) return Boolean;

end Placeholder_Relations;
