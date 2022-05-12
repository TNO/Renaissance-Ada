with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Match_Accepters;             use Match_Accepters;

package Match_Accepters_Mark_Aware is

   type Match_Accepter_Mark_Aware is new Match_Accepter with private;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_Mark_Aware;
      M_P  : Match_Pattern) return Boolean;

   function Make_Match_Accepter_Mark_Aware
     (M_A : Match_Accepter'Class)
      return Match_Accepter_Mark_Aware;

private

   type Any_Match_Accepter is not null access Match_Accepter'Class;

   type Match_Accepter_Mark_Aware is new Match_Accepter with record
      A_Match_Accepter : Any_Match_Accepter;
   end record;

   function Make_Match_Accepter_Mark_Aware
     (M_A : Match_Accepter'Class)
      return Match_Accepter_Mark_Aware
   is
      (A_Match_Accepter => new Match_Accepter'Class'(M_A));

end Match_Accepters_Mark_Aware;
