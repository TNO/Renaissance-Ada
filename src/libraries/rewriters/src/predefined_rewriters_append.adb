package body Predefined_Rewriters_Append is

   function Append_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector
        (Rewriter_Ampersand_Prefer_Append.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewriter_Append_To_Unbounded_String.Matching_Nodes (Unit));
      return Result;
   end Append_Rewrite_Context;

end Predefined_Rewriters_Append;
