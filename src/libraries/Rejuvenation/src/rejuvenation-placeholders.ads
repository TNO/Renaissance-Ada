--  Internal package

--  A placeholder is a hole / wild card for AST matching.
--  A placeholder can also act as backreference:
--    * when a placeholder occurs multiple times within a single find pattern,
--      these parts should be identical to match
--    * when a placeholder occurs within a replace pattern,
--      it refer to the part that matches that placeholder in the find pattern.

--  There are two types of placeholders:
--    $S_ : denotes exactly one node
--    $M_ : denotes zero or more nodes

with String_Sets; use String_Sets;

package Rejuvenation.Placeholders is

   function Is_Placeholder_Name (Name : String) return Boolean;
   --  Is the provided name a placeholder name?
   --  A placeholder name is an identifier that starts with '$M_' or '$S_'

   function Is_Single_Placeholder_Name (Name : String) return Boolean;
   function Is_Multiple_Placeholder_Name (Name : String) return Boolean;

   function Is_Placeholder (Node : Ada_Node'Class) return Boolean;
   --  Is the provide node a placeholder?

   function Is_Single_Placeholder (Node : Ada_Node'Class) return Boolean;

   function Is_Multiple_Placeholder (Node : Ada_Node'Class) return Boolean;

   function Get_Placeholder_Name (Node : Ada_Node'Class) return String with
      Pre  => Is_Placeholder (Node),
      Post => Is_Placeholder_Name (Get_Placeholder_Name'Result);
      --  Get the name of placeholder.

   function Get_Placeholders
     (Node : Ada_Node'Class) return Node_List.Vector with
      Post =>
      (for all Element of Get_Placeholders'Result => Is_Placeholder (Element));
      --  Provide a list with all placeholder nodes within the given node.
      --  The list might contain multiple placeholder nodes that share the
      --  same placeholder name. These nodes, of course, differ in their
      --  position within the text / AST tree.
      --  TODO: should we promiss the nodes appear in their textual order?

   function Get_Placeholder_Names (Node : Ada_Node'Class) return Set;
   --   Provide a set with all placeholder names within the given node.

end Rejuvenation.Placeholders;
