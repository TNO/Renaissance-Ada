with Libadalang.Analysis;        use Libadalang.Analysis;
with Rejuvenation;               use Rejuvenation;
with Rejuvenation.Patterns;      use Rejuvenation.Patterns;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Match_Accepters;            use Match_Accepters;
with Match_Accepters_All;        use Match_Accepters_All;

package Rewriters_Find_And_Replace_Combined is

   type Rewriters_Find_And_Replace_Combined is
     new Rewriter_Find_And_Replace with private;

   overriding function Rewrite
     (RFR : Rewriters_Find_And_Replace_Combined; Unit : in out Analysis_Unit)
      return Boolean;

   overriding function Get_Find_Pattern
     (RFR : Rewriters_Find_And_Replace_Combined) return Pattern;

   overriding function Get_Replace_Pattern
     (RFR : Rewriters_Find_And_Replace_Combined) return Pattern;

   overriding function Get_Match_Accepter
     (RFR : Rewriters_Find_And_Replace_Combined) return Match_Accepter'Class;

   overriding function Matching_Nodes
     (RFR : Rewriters_Find_And_Replace_Combined; Unit : Analysis_Unit)
      return Node_List.Vector;

   function Make_Rewriter_Find_And_Replace
     (Base         : Rewriter_Find_And_Replace_Basic;
      Accept_Match : Match_Accepter'Class)
      return Rewriter_Find_And_Replace_Combined;

private

   type Any_Match_Accepter is not null access Match_Accepter'Class;

   type Rewriter_Find_And_Replace_Basic is new Rewriter_Find_And_Replace with
   record
      F_Base           : Rewriter_Find_And_Replace_Basic;
      A_Match_Accepter : Any_Match_Accepter;
   end record;

   overriding function Get_Find_Pattern
     (RFR : Rewriters_Find_And_Replace_Combined) return Pattern is
     (F_Base.Get_Find_Pattern);

   overriding function Get_Replace_Pattern
     (RFR : Rewriters_Find_And_Replace_Combined) return Pattern is
     (F_Base.Get_Replace_Pattern);

   function Make_Rewriter_Find_And_Replace
     (Base         : Rewriter_Find_And_Replace_Basic;
      Accept_Match : Match_Accepter'Class)
      return Rewriter_Find_And_Replace_Combined is
     (Rewriter_Find_And_Replace with Base,
      new Match_Accepter'Class'(Accept_Match));

end Rewriters_Find_And_Replace_Combined;
