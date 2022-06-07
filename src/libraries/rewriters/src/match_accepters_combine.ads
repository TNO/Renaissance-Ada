with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Match_Accepters;             use Match_Accepters;

package Match_Accepters_Combine is

   type Match_Accepter_Combine is new Match_Accepter with private;

   overriding function Is_Match_Acceptable
     (M_A : Match_Accepter_Combine;
      M_P : Match_Pattern) return Boolean;

   function Make_Match_Accepter_Combine
     (Left, Right : Match_Accepter'Class)
     return Match_Accepter_Combine;

private

   type Match_Accepter_Access is not null access Match_Accepter'Class;

   type Match_Accepter_Combine is new Match_Accepter with
      record
         A_Left, A_Right : Match_Accepter_Access;
      end record;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_Combine;
      M_P  : Match_Pattern) return Boolean is
     (M_A.A_Left.Is_Match_Acceptable (M_P)
      and then M_A.A_Right.Is_Match_Acceptable (M_P));

   function Make_Match_Accepter_Combine
     (Left, Right : Match_Accepter'Class)
     return Match_Accepter_Combine
   is
     (A_Left => new Match_Accepter'Class'(Left),
      A_Right => new Match_Accepter'Class'(Right));

end Match_Accepters_Combine;
