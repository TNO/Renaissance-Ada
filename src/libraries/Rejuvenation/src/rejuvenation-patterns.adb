with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Rejuvenation.Patterns is

   function Make_Pattern
     (Fragment : String; Rule : Grammar_Rule) return Pattern
   is
      Unit : constant Analysis_Unit := Analyze_Fragment (Fragment, Rule);
   begin
      return (To_Unbounded_String (Fragment), Rule, Unit);
   end Make_Pattern;

end Rejuvenation.Patterns;
