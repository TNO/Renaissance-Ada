package body Predefined_Rewriters_Boolean_Expression_De_Morgan is

   function De_Morgan_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector (Rewrite_De_Morgan_Not_And.Matching_Nodes (Unit));
      Result.Append_Vector (Rewrite_De_Morgan_Not_Or.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewrite_De_Morgan_Not_All_Range.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewrite_De_Morgan_Not_All_Elements.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewrite_De_Morgan_Not_Some_Range.Matching_Nodes (Unit));
      Result.Append_Vector
        (Rewrite_De_Morgan_Not_Some_Elements.Matching_Nodes (Unit));

      return Result;
   end De_Morgan_Rewrite_Context;

end Predefined_Rewriters_Boolean_Expression_De_Morgan;
