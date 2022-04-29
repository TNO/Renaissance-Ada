with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;

package Rewriters_Node_Utils is

   function Preceeding_Node (Node : Ada_Node'Class) return Ada_Node;

   function Last_Token_Of_Preceeding_Node
     (Node : Ada_Node'Class)
     return Token_Reference;

end Rewriters_Node_Utils;
