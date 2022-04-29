package body Rewriters_Node_Utils is

   function Preceeding_Node (Node : Ada_Node'Class) return Ada_Node is
      Previous_Sibling : constant Ada_Node := Node.Previous_Sibling;
   begin
      if Previous_Sibling.Is_Null then
         declare
            Parent : constant Ada_Node := Node.Parent;
         begin
            if Parent.Is_Null then
               return No_Ada_Node;
            else
               return Preceeding_Node (Parent);
            end if;
         end;
      else
         if Is_Ghost (Previous_Sibling) then
            return Preceeding_Node (Previous_Sibling);
         else
            return Previous_Sibling;
         end if;
      end if;
   end Preceeding_Node;

   function Last_Token_Of_Preceeding_Node
     (Node : Ada_Node'Class) return Token_Reference
   is
      Previous_Sibling : constant Ada_Node := Node.Previous_Sibling;
   begin
      if Previous_Sibling.Is_Null then
         if Node.Parent.Is_Null then
            return No_Token;
         else
            return Last_Token_Of_Preceeding_Node (Node.Parent);
         end if;
      else
         return Previous_Sibling.Token_End;
      end if;
   end Last_Token_Of_Preceeding_Node;

end Rewriters_Node_Utils;
