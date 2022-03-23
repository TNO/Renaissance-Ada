package Rejuvenation.Navigation is

   function Is_Ancestor (Node1, Node2 : Ada_Node'Class) return Boolean;
--  Is the first node an ancestor of the second node (but not the node itself)?

   function Is_Reflexive_Ancestor
     (Node1, Node2 : Ada_Node'Class) return Boolean;
   --  Is the first node an ancestor of the second node
   --  (possibly the node itself)?
   --  Note: Null is not a Reflexive Ancestor of itself...
   --        (is this what we want?)

   function Get_Ancestor
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Ada_Node;
   --  Return the first ancestor of the AST node that match the Predicate
   --  (but not the node itself), or No_Ada_Node.
   function Get_Reflexive_Ancestor
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Ada_Node;
   --  Return the first ancestor of the AST node that match the Predicate
   --  (possibly the node itself), or No_Ada_Node.

   function Get_Ancestor_Of_Type
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Ada_Node;
   --  Return the first ancestor of the AST node of type Node_Kind
   --  (but not the node itself), or No_Ada_Node.
   function Get_Reflexive_Ancestor_Of_Type
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Ada_Node;
   --  Return the first ancestor of the AST node of type Node_Kind
   --  (possibly the node itself), or No_Ada_Node.

end Rejuvenation.Navigation;
