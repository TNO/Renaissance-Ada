with Libadalang.Analysis;            use Libadalang.Analysis;
with Rejuvenation;                   use Rejuvenation;
with Rejuvenation.Find_And_Replacer; use Rejuvenation.Find_And_Replacer;
with Rejuvenation.Match_Patterns;    use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;          use Rejuvenation.Patterns;

with Rewriters;       use Rewriters;

package Rewriters_Find_And_Replace is

   type Match_Accepter is not null access function
     (Match : Match_Pattern) return Boolean;

   type Rewriter_Find_And_Replace is new Rewriter with private;

   overriding procedure Rewrite
     (RFR : Rewriter_Find_And_Replace; Unit : in out Analysis_Unit);

   function Find_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern;

   function Replace_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern;

   function Accept_Match
     (RFR : Rewriter_Find_And_Replace)
      return Match_Accepter;

   function Matching_Nodes
     (RFR : Rewriter_Find_And_Replace; Unit : Analysis_Unit)
      return Node_List.Vector;
   --  Vector of nodes that match both
   --  the find pattern and the accept_match function

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match                  : Match_Accepter :=
        Accept_All_Matches'Access)
      return Rewriter_Find_And_Replace;
   --  Note: When rewriting all instances of a particular warning
   --  detected by your favorite linter, such as GNATcheck and CodePeer,
   --  you don't have to reimplement the semantic check of that warning
   --  to accept a match, since
   --  you can just check whether the location of the found instance occurs
   --  in the list of reported locations by the linter.

   --  TODO: Enforce compatibility between find and replace pattern

private

   type Rewriter_Find_And_Replace is new Rewriter with record
      F_Find_Pattern    : Pattern;
      F_Replace_Pattern : Pattern;
      F_Match_Accepter  : Match_Accepter;
   end record;

   function Find_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern is
     (RFR.F_Find_Pattern);

   function Replace_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern is
     (RFR.F_Replace_Pattern);

   function Accept_Match
     (RFR : Rewriter_Find_And_Replace)
      return Match_Accepter is
     (RFR.F_Match_Accepter);

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match                  : Match_Accepter :=
        Accept_All_Matches'Access)
      return Rewriter_Find_And_Replace is
     (Rewriter with Find_Pattern, Replace_Pattern, Accept_Match);

end Rewriters_Find_And_Replace;
