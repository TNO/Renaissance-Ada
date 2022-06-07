package body Predefined_Rewriters_Prefer_If_Expression is

   function If_Expression_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector
        (Rewriter_If_Stmt_Subprogram_Single_Diffent_Argument.Matching_Nodes
           (Unit));
      Result.Append_Vector (Rewriter_If_Stmt_Assignment.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_If_Stmt_Return.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_If_Stmt_Return_Stmt.Matching_Nodes (Unit));
      return Result;
   end If_Expression_Rewrite_Context;

end Predefined_Rewriters_Prefer_If_Expression;
