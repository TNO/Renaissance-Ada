with Libadalang.Analysis;        use Libadalang.Analysis;
with Rejuvenation;               use Rejuvenation;
with Rejuvenation.Patterns;      use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Match_Accepters;            use Match_Accepters;
with Match_Accepters_All;        use Match_Accepters_All;

package Rewriters_Find_And_Replace_Basic is

   type Rewriter_Find_And_Replace_Basic is
     new Rewriter_Find_And_Replace with private;

   overriding function Rewrite
     (RFR : Rewriter_Find_And_Replace_Basic; Unit : in out Analysis_Unit)
      return Boolean;

   overriding function Get_Find_Pattern
     (RFR : Rewriter_Find_And_Replace_Basic) return Pattern;

   overriding function Get_Replace_Pattern
     (RFR : Rewriter_Find_And_Replace_Basic) return Pattern;

   overriding function Get_Match_Accepter
     (RFR : Rewriter_Find_And_Replace_Basic) return Match_Accepter'Class;

   overriding function Matching_Nodes
     (RFR : Rewriter_Find_And_Replace_Basic; Unit : Analysis_Unit)
      return Node_List.Vector;
   --  Vector of nodes that match both
   --  the find pattern and the accept_match function

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match : Match_Accepter'Class := Make_Match_Accepter_All)
      return Rewriter_Find_And_Replace_Basic;
   --  Note: When rewriting all instances of a particular warning
   --  detected by your favorite linter, such as GNATcheck and CodePeer,
   --  you don't have to reimplement the semantic check of that warning
   --  to accept a match, since
   --  you can just check whether the location of the found instance occurs
   --  in the list of reported locations by the linter.
   --
   --  TODO: Enforce compatibility between find and replace pattern

private

   type Any_Match_Accepter is not null access Match_Accepter'Class;

   type Rewriter_Find_And_Replace_Basic is new Rewriter_Find_And_Replace with
   record
      F_Find_Pattern    : Pattern;
      F_Replace_Pattern : Pattern;
      A_Match_Accepter  : Any_Match_Accepter;
   end record;

   overriding function Get_Find_Pattern
     (RFR : Rewriter_Find_And_Replace_Basic) return Pattern is
     (RFR.F_Find_Pattern);

   overriding function Get_Replace_Pattern
     (RFR : Rewriter_Find_And_Replace_Basic) return Pattern is
     (RFR.F_Replace_Pattern);

   overriding function Get_Match_Accepter
     (RFR : Rewriter_Find_And_Replace_Basic) return Match_Accepter'Class is
     (RFR.A_Match_Accepter.all);

   function Make_Rewriter_Find_And_Replace
     (Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match : Match_Accepter'Class := Make_Match_Accepter_All)
      return Rewriter_Find_And_Replace_Basic is
     (Rewriter_Find_And_Replace with Find_Pattern, Replace_Pattern,
      new Match_Accepter'Class'(Accept_Match));

end Rewriters_Find_And_Replace_Basic;
