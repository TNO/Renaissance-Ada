package Rejuvenation.Node_Locations is

   type Node_Location is (No_Trivia, Trivia_On_Same_Line, All_Trivia);
   --  Between two adjacent nodes in the AST, trivia
   --  (i.e. white space and comment) can occur.
   --  This raises the question: Where does a Node start / end?
   --  No_Trivia           : Trivia before / after the node is
   --                        not considered part of that node
   --  Trivia_On_Same_Line : Trivia before / after the node
   --                        on the same line is considered part
   --                        of that node
   --  All_Trivia          : Trivia before / after the node is
   --                        considered part of that node

   --  TODO: Investigate (suggestion of Arjan) is there a need
   --        for the following?
   --  Trivia_Until_Empty_Line: Trivia before / after the node
   --                           until an empty line is considered
   --                           part of that node
   --  TODO: Investigate is there a need for the following?
   --  All_Trivia_When_Keyword : when each node has all trivia
   --                            before and after him,
   --                            we will duplicate trivia.
   --                            E.g. "Node1; -- comment Node2;"
   --  when we limit trivia to one direction (before / after node),
   --  we still miss trivia (before/after) keywords e.g.
   --  (if -- missed_after Condition -- missed_before then ...)

   function Start_Offset (Node : Ada_Node'Class;
                          Before : Node_Location := No_Trivia)
                          return Positive;

   function End_Offset (Node : Ada_Node'Class;
                        After : Node_Location := No_Trivia)
                        return Natural;

end Rejuvenation.Node_Locations;
