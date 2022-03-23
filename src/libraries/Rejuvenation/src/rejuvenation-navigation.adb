package body Rejuvenation.Navigation is

   function Is_Ancestor (Node1, Node2 : Ada_Node'Class) return Boolean is
   begin
      return
        not Node2.Is_Null and then Is_Reflexive_Ancestor (Node1, Node2.Parent);
   end Is_Ancestor;

   function Is_Reflexive_Ancestor
     (Node1, Node2 : Ada_Node'Class) return Boolean
   is
   begin
      return
        not Node2.Is_Null
        and then
        (Node1 = Node2 or else Is_Reflexive_Ancestor (Node1, Node2.Parent));
   end Is_Reflexive_Ancestor;

   function Get_Ancestor
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Ada_Node is
     (if Node.Is_Null then Node.As_Ada_Node
      else Get_Reflexive_Ancestor (Node.Parent, Predicate));

   function Get_Reflexive_Ancestor
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Ada_Node
   is
      Running_Node : Ada_Node := Node.As_Ada_Node;
   begin
      while not Running_Node.Is_Null and then not Predicate (Running_Node) loop
         Running_Node := Running_Node.Parent;
      end loop;
      return Running_Node;
   end Get_Reflexive_Ancestor;

   function Get_Ancestor_Of_Type
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type) return Ada_Node
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
      begin
         return Node.Kind = Node_Kind;
      end Predicate;
   begin
      return Get_Ancestor (Node, Predicate'Access);
   end Get_Ancestor_Of_Type;

   function Get_Reflexive_Ancestor_Of_Type
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type) return Ada_Node
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
      begin
         return Node.Kind = Node_Kind;
      end Predicate;
   begin
      return Get_Reflexive_Ancestor (Node, Predicate'Access);

   end Get_Reflexive_Ancestor_Of_Type;

end Rejuvenation.Navigation;
