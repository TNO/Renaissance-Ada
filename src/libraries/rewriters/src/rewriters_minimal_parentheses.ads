with Libadalang.Analysis;     use Libadalang.Analysis;
with Rejuvenation;            use Rejuvenation;
with Rewriters;               use Rewriters;

package Rewriters_Minimal_Parentheses is
   --  TODO: move to find_and_replace rewriter using pattern "($S_EXPR)"

   type Node_Accepter is not null access function
     (Node : Ada_Node) return Boolean;

   function Accept_All_Nodes (Node : Ada_Node) return Boolean;

   function Are_Parentheses_Necessary (P_E : Paren_Expr) return Boolean;
   --  Are Parentheses necessary for this Paren_Expr?

   type Rewriter_Minimal_Parentheses is new Rewriter with private;

   function Accept_Node
     (RMP : Rewriter_Minimal_Parentheses)
      return Node_Accepter;

   overriding function Rewrite
     (RMP : Rewriter_Minimal_Parentheses; Unit : in out Analysis_Unit)
   return Boolean;

   function Matching_Nodes
     (RMP : Rewriter_Minimal_Parentheses; Unit : Analysis_Unit)
      return Node_List.Vector;
   --  Vector of nodes that match both
   --  Are_Parentheses_Necessary, hence node has kind Paren_Expr,
   --  and the accept_node function

   function Make_Rewriter_Minimal_Parentheses
     (Accept_Node : Node_Accepter := Accept_All_Nodes'Access)
      return Rewriter_Minimal_Parentheses;

private

   function Accept_All_Nodes (Node : Ada_Node) return Boolean is (True);

   type Rewriter_Minimal_Parentheses is new Rewriter
   with record
      F_Node_Accepter  : Node_Accepter;
   end record;

   function Accept_Node
     (RMP : Rewriter_Minimal_Parentheses)
      return Node_Accepter is
     (RMP.F_Node_Accepter);

   function Make_Rewriter_Minimal_Parentheses
     (Accept_Node : Node_Accepter := Accept_All_Nodes'Access)
      return Rewriter_Minimal_Parentheses is
     (F_Node_Accepter => Accept_Node);

end Rewriters_Minimal_Parentheses;
