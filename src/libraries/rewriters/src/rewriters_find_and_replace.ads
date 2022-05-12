with Libadalang.Analysis;   use Libadalang.Analysis;
with Rejuvenation;          use Rejuvenation;
with Rejuvenation.Patterns; use Rejuvenation.Patterns;

with Rewriters;       use Rewriters;
with Match_Accepters; use Match_Accepters;

package Rewriters_Find_And_Replace is

   type Rewriter_Find_And_Replace is interface and Rewriter;

   overriding function Rewrite
     (RFR : Rewriter_Find_And_Replace; Unit : in out Analysis_Unit)
      return Boolean is abstract;

   function Get_Find_Pattern
     (RFR : Rewriter_Find_And_Replace) return Pattern is abstract;

   function Get_Replace_Pattern
     (RFR : Rewriter_Find_And_Replace) return Pattern is abstract;

   function Get_Match_Accepter
     (RFR : Rewriter_Find_And_Replace) return Match_Accepter'Class is abstract;

   function Matching_Nodes
     (RFR : Rewriter_Find_And_Replace; Unit : Analysis_Unit)
      return Node_List.Vector is abstract;
   --  Vector of nodes that match both
   --  the find pattern and the accept_match function

end Rewriters_Find_And_Replace;
