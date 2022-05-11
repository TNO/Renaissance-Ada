with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;

package Match_Accepters is

   type Match_Accepter is interface;

   function Is_Match_Acceptable
     (M_A  : Match_Accepter;
      M_P  : Match_Pattern) return Boolean is abstract;
   --  Is Match Acceptable?
   --  Semantical acceptance test after syntactical match

end Match_Accepters;
