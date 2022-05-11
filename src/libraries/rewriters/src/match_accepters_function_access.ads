with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Match_Accepters;             use Match_Accepters;

package Match_Accepters_Function_Access is
   --  TODO:  create Match_Accepters for some simple regular occuring matches
   --  such that is match_accepter will be less often used.

   type Access_Function_Is_Match_Acceptable is
   not null access function (M_P : Match_Pattern) return Boolean;

   type Match_Accepter_Function_Access is new Match_Accepter with private;

   overriding function Is_Match_Acceptable
     (M_A : Match_Accepter_Function_Access;
      M_P : Match_Pattern) return Boolean;

   function Make_Match_Accepter_Function_Access
     (A_F : Access_Function_Is_Match_Acceptable)
     return Match_Accepter_Function_Access;

private

   type Match_Accepter_Function_Access is new Match_Accepter with
      record
         F_Function : Access_Function_Is_Match_Acceptable;
      end record;

   overriding function Is_Match_Acceptable
     (M_A  : Match_Accepter_Function_Access;
      M_P  : Match_Pattern) return Boolean is
     (M_A.F_Function (M_P));

   function Make_Match_Accepter_Function_Access
     (A_F : Access_Function_Is_Match_Acceptable)
      return Match_Accepter_Function_Access
   is
      (F_Function => A_F);

end Match_Accepters_Function_Access;
