with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Match_Accepters;             use Match_Accepters;

package Match_Accepters_All is

   type Match_Accepter_All is new Match_Accepter with private;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_All;
      M_P  : Match_Pattern) return Boolean;

   function Make_Match_Accepter_All return Match_Accepter_All;

private

   type Match_Accepter_All is new Match_Accepter with null record;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_All;
      M_P  : Match_Pattern) return Boolean is
     (True);

   function Make_Match_Accepter_All return Match_Accepter_All
   is
      (null record);

end Match_Accepters_All;
