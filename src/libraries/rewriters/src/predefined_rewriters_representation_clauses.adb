package body Predefined_Rewriters_Representation_Clauses is

   function Representation_Clauses_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector (Rewriter_For_Attribute_Use.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Attribute_Use_Aliased.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Attribute_Use_Array.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Attribute_Use_Pragma_Var.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_For_Attribute_Use_Pragma_All.Matching_Nodes (Unit));

      return Result;
   end Representation_Clauses_Rewrite_Context;

end Predefined_Rewriters_Representation_Clauses;
