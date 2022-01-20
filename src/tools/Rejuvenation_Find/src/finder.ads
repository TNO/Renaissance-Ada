with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;

package Finder is

   function Find (Node : Ada_Node'Class;
                  P    : Pattern)
                  return Match_Pattern_List.Vector;
   --  Return all recursively nested nodes from the AST instance
   --  that fully match the AST pattern.

end Finder;
