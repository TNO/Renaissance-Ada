package body Predefined_Rewriters_Declaration_Simplify is

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
            return Parent;
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

   function Declarations_Combine_Rewrite_Context
     (Unit : Analysis_Unit) return Node_List.Vector
   is
   begin
      return Context (Rewriter_Declarations_Combine_Step.Matching_Nodes (Unit));
   end Declarations_Combine_Rewrite_Context;

end Predefined_Rewriters_Declaration_Simplify;
