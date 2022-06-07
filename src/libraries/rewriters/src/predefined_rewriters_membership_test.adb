package body Predefined_Rewriters_Membership_Test is

   function Context (Node : Ada_Node) return Ada_Node;
   function Context (Node : Ada_Node) return Ada_Node
   is
   begin
      if Node.Is_Null then
         return Node;
      else
         declare
            Parent : constant Ada_Node := Node.Parent;
         begin
            if Parent.Is_Null or else Parent.Kind /= Ada_Bin_Op then
               return Node;
            else
               return Context (Parent);
            end if;
         end;
      end if;
   end Context;

   function Context (Nodes : Node_List.Vector) return Node_List.Vector;
   function Context (Nodes : Node_List.Vector) return Node_List.Vector
   is
      Return_Value : Node_List.Vector;
   begin
      for Node of Nodes loop
         Return_Value.Append (Context (Node));
      end loop;
      return Return_Value;
   end Context;

   function Membership_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
      Result : Node_List.Vector;
   begin
      Result.Append_Vector
        (Context (Rewriter_Equals_To_In_Range.Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_In_Range_And_Equal_Step.Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_Equal_And_In_Range_Step.Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_In_Ranges_Step.Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context (Rewriter_Differents_To_Not_In_Range.Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_Not_In_Range_And_Different_Step
            .Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_Different_And_Not_In_Range_Step
            .Matching_Nodes (Unit)));

      Result.Append_Vector
        (Context
           (Rewriter_Combine_Not_In_Ranges_Step
            .Matching_Nodes (Unit)));

      return Result;
   end Membership_Rewrite_Context;

end Predefined_Rewriters_Membership_Test;
