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

package Predefined_Rewriters_Append is

   function Accept_Var_Unbounded_String (Match : Match_Pattern) return Boolean
   is
      (Is_Unbounded_String (Match, "$S_Var"));

   function Accept_Append_To_Unbounded_String
     (Match : Match_Pattern)
      return Boolean
   is
     (Is_String_Expression (Match, "$S_Expr") and then
      Is_Referenced_Decl_Defined_In_AStrUnb
        (Match.Get_Nodes.First_Element.As_Call_Stmt.F_Call));

   Rewriter_Append_To_Unbounded_String :
     aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern
          ("Append ($S_Var, To_Unbounded_String ($M_Source => $S_Expr));",
           Call_Stmt_Rule),
        Make_Pattern
          ("Append ($S_Var, $S_Expr);", Call_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Append_To_Unbounded_String'Access));

   Rewriter_Ampersand_Prefer_Append :
   aliased constant Rewriter_Find_And_Replace :=
     Make_Rewriter_Find_And_Replace
       (Make_Pattern ("$S_Var := $S_Var & $S_Tail;", Assignment_Stmt_Rule),
        Make_Pattern ("Append ($S_Var, $S_Tail);", Call_Stmt_Rule),
        Make_Match_Accepter_Function_Access
          (Accept_Var_Unbounded_String'Access));

   Rewriter_Append : aliased constant Rewriter_Sequence :=
     Make_Rewriter_Sequence
       (Rewriter_Ampersand_Prefer_Append & Rewriter_Append_To_Unbounded_String);
   --  Rewriter for patterns to improve the usage of the `Append` function.

   function Append_Rewrite_Context
     (Unit : Analysis_Unit)
      return Node_List.Vector;

end Predefined_Rewriters_Append;
