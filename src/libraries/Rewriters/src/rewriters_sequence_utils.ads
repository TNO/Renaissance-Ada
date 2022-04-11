with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;
with Rewriters_Sequence;          use Rewriters_Sequence;

package Rewriters_Sequence_Utils is

   function Rewrite (RS        : Rewriters_Sequence.Vector;
                     Node      : Ada_Node'Class;
                     Top_Level : Boolean      := False;
                     Rule      : Grammar_Rule := Default_Grammar_Rule)
                     return String;

   function Rewrite_Context
     (RS   : Rewriters_Sequence.Vector;
      Node : Ada_Node'Class)
      return Ada_Node
     with Post => Is_Reflexive_Ancestor (Rewrite_Context'Result, Node);

end Rewriters_Sequence_Utils;
