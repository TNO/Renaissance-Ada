with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Match_Accepters;             use Match_Accepters;

package Match_Accepters_Marked is

   type Match_Accepter_Marked is new Match_Accepter with private;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_Marked;
      M_P  : Match_Pattern) return Boolean;

   function Make_Match_Accepter_Marked return Match_Accepter_Marked;

private

   type Match_Accepter_Marked is new Match_Accepter with null record;

   function Make_Match_Accepter_Marked return Match_Accepter_Marked
   is
      (null record);

end Match_Accepters_Marked;
