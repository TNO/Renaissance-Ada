with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;

package Rejuvenation.Finder is

   --  Methods to find nodes of a certain Node_Kind or nodes that match certain
   --  AST patterns.

   -- Externally-visible data structures -------

   package Match_Pattern_List is new Vectors (Positive, Match_Pattern);

   type Pattern_Array is
         array (Positive range <>) of Pattern;

   package Pattern_Vectors is new Vectors (Positive, Pattern);

   --  Data type for list of Match_Pattern.
   Pattern_Is_No_List_Exception : exception;
   --  Exception that indicates that the pattern parameter was
   --  not an Ada_List (as required).

   -- Find Predicate --------

   function Find
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Node_List.Vector;
   --  Return all recursively nested nodes from the AST instance
   --  that match the Predicate.

   function Find_Non_Contained
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Node_List.Vector;
   --  Return all nodes from the AST instance that match the Predicate,
   --  but without nodes that are contained / recursively nested
   --  in other returned nodes.

   -- Find Node_Kind --------

   function Find
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Node_List.Vector;
   --  Return all recursively nested nodes from the AST instance
   --  that match the Node_Kind.

   function Find_Non_Contained
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Node_List.Vector;
   --  Return all nodes from the AST instance that match the Node_Kind,
   --  but without nodes that are contained / recursively nested
   --  in other returned nodes.

   function Find_First
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Ada_Node;
   --  Return the first recursively nested node from the AST instance
   --  that match the Node_Kind.

   function Find_Sub_List
     (Node : Ada_Node'Class; Node_Kinds : Node_Kind_Type_Array)
      return Node_List_List.Vector;
   --  Return all recursively nested subsequences of nodes
   --  from the AST instance that fully match the node kinds.

   -- Find Match_Pattern --------
   function Find_Full
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector;
   --  Return all recursively nested nodes from the AST instance
   --  that fully match the AST pattern.

   function Find_Non_Contained_Full
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector;
   --  Return all nodes from the AST instance that fully match the AST pattern,
   --  but without nodes that are contained / recursively nested
   --  in other returned nodes.

   function Find_First_Full
     (Node   :     Ada_Node'Class; Find_Pattern : Pattern;
      Result : out Match_Pattern) return Boolean;
   --  Return the first recursively nested node from the AST instance that
   --  fully match the AST pattern.

   function Find_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector;
   --  Return all recursively nested subsequences of nodes
   --  from the AST instance that fully match the AST pattern.

   function Find_Non_Contained_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector;
   --  Return all nodes from the AST instance that fully match the AST pattern,
   --  but without nodes that are contained / recursively nested
   --  in other returned nodes.

   function Find_Full
     (Node : Ada_Node'Class; Find_Patterns : Pattern_Array)
      return Match_Pattern_List.Vector;
   --  Return all recursively nested nodes from the AST instance that
   --  fully match any of the AST patterns.
   --  Note: when a Node matches multiple patterns,
   --        it will occur multiple times in the result.

private

   function Find_Predicate
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean;
      Next : Visit_Status) return Node_List.Vector;

   function Find_NK_Sub_List
     (Node : Ada_Node'Class; Node_Kinds : Node_Kind_Type_Array)
      return Node_List_List.Vector;

   function Find_MP
     (Node : Ada_Node'Class; Pattern : Ada_Node;
      Next : Visit_Status) return Match_Pattern_List.Vector;

   type Containment is (Contained, Non_Contained);

   function Find_MP_Sub_List
     (Node : Ada_Node'Class;
      Pattern : Ada_Node_Array;
      Next : Containment)
      return Match_Pattern_List.Vector;

end Rejuvenation.Finder;
