package body Ada_Node_Interval is

   function Get_Interval_With_Trivia (Node : Ada_Node) return Interval is
        (Make_Interval
           (Start_Offset (Node, Before),
            End_Offset (Node, After))
        );

end Ada_Node_Interval;
