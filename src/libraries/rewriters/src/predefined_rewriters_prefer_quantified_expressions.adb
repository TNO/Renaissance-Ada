package body Predefined_Rewriters_Prefer_Quantified_Expressions is

   function Quantified_Expressions_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector
        (Rewriter_For_All_Range_And_Then.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_All_Elements_And_Then.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Range_Or_Else.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Elements_Or_Else.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_For_All_Range_Exit.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_All_Elements_Exit.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Range_Exit.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Elements_Exit.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_All_Range_Return.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_All_Elements_Return.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Range_Return.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Elements_Return.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_For_All_Range_All.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_All_Elements_All.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_For_Some_Range_All.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Some_Elements_All.Matching_Nodes (Unit));

      return Result;
   end Quantified_Expressions_Rewrite_Context;

end Predefined_Rewriters_Prefer_Quantified_Expressions;
