package Rejuvenation.Indentation is

   No_Indentation : constant Integer := -1;

   function Indentation_Of_Node (Node : Ada_Node'Class) return Integer;
   --  Note: No_Indentation signals that another non-white-space element
   --  is earlier on the same line

   function Node_On_Separate_Lines (Node : Ada_Node'Class) return Boolean;
   --  Is Node on separate lines defined.
   --  Useful for line-based pretty printing (e.g. offered by gnatpp).

end Rejuvenation.Indentation;
