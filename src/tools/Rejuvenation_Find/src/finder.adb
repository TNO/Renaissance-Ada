with Libadalang.Common; use Libadalang.Common;

package body Finder is

   function Find
     (Node : Ada_Node'Class;
      P    : Pattern)
      return Match_Pattern_List.Vector is
     (if P.As_Ada_Node.Kind in Ada_Ada_List
      then Find_Sub_List (Node, P)
      else Find_Full (Node, P));

end Finder;
