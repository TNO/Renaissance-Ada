package body Predefined_Rewriters_Not is

   function Matching_Not_Nodes (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector (Rewriter_Not_Not.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Equal.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Different.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_In.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Not_In.Matching_Nodes (Unit));

      Result.Append_Vector (Rewriter_Not_Greater_Than.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Greater_Equal.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Less_Than.Matching_Nodes (Unit));
      Result.Append_Vector (Rewriter_Not_Less_Equal.Matching_Nodes (Unit));
      return Result;
   end Matching_Not_Nodes;

end Predefined_Rewriters_Not;
