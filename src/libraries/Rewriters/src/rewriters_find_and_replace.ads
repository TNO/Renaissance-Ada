with Libadalang.Analysis;            use Libadalang.Analysis;
with Rejuvenation.Find_And_Replacer; use Rejuvenation.Find_And_Replacer;
with Rejuvenation.Navigation;        use Rejuvenation.Navigation;
with Rejuvenation.Patterns;          use Rejuvenation.Patterns;
with Rewriters;                      use Rewriters;
with Rewriters_Sequence;             use Rewriters_Sequence;

package Rewriters_Find_And_Replace is

   type Rewriter_Find_And_Replace is new Rewriter with private;

   overriding function Rewrite
     (RFR       : Rewriter_Find_And_Replace; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String;

   overriding function Rewrite_Context
     (RFR : Rewriter_Find_And_Replace; Node : Ada_Node'Class)
      return Ada_Node with
      Post => Is_Reflexive_Ancestor (Rewrite_Context'Result, Node);

   function Find_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern;

   function Replace_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern;

   function Accept_Match
     (RFR : Rewriter_Find_And_Replace) return Match_Accepter;
   --  Note: When rewriting all instances of a particular warning
   --  detected by your favorite linter, such as GNATcheck and CodePeer,
   --  you don't have to reimplement the semantic check of that warning
   --  to accept a match, since
   --  you can just check whether the location of the found instance occurs
   --  in the list of reported locations by the linter.

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match : Match_Accepter         := Accept_All_Matches'Access;
      Rewriters : Rewriters_Sequence.Vector := Rewriters_Sequence.Empty_Vector)
      return Rewriter_Find_And_Replace;
   --  TODO: Enforce compatibility between find and replace pattern

private

   type Rewriter_Find_And_Replace is new Rewriter with record
      F_Find_Pattern    : Pattern;
      F_Replace_Pattern : Pattern;
      F_Match_Accepter  : Match_Accepter;
      F_Rewriters       : Rewriters_Sequence.Vector;
   end record;

   function Find_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern is
     (RFR.F_Find_Pattern);

   function Replace_Pattern (RFR : Rewriter_Find_And_Replace) return Pattern is
     (RFR.F_Replace_Pattern);

   function Accept_Match
     (RFR : Rewriter_Find_And_Replace) return Match_Accepter is
     (RFR.F_Match_Accepter);

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match : Match_Accepter            := Accept_All_Matches'Access;
      Rewriters : Rewriters_Sequence.Vector := Rewriters_Sequence.Empty_Vector)
      return Rewriter_Find_And_Replace is
     (Rewriter with Find_Pattern, Replace_Pattern, Accept_Match, Rewriters);

end Rewriters_Find_And_Replace;
