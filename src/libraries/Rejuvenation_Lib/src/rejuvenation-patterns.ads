private with Ada.Strings.Unbounded;

package Rejuvenation.Patterns is

   type Pattern is tagged private;

   function Get_String (P : Pattern) return String;

   function Get_Rule (P : Pattern) return Grammar_Rule;

   function As_Ada_Node (P : Pattern) return Ada_Node;

   function Make_Pattern
     (Fragment : String; Rule : Grammar_Rule) return Pattern;

private
   use Ada.Strings.Unbounded;

   type Pattern is tagged record
      UStr     : Unbounded_String;
      Rule     : Grammar_Rule;
      Unit     : Analysis_Unit;
   end record;

   function Get_String (P : Pattern) return String is (To_String (P.UStr));

   function Get_Rule (P : Pattern) return Grammar_Rule is (P.Rule);

   function As_Ada_Node (P : Pattern) return Ada_Node is (P.Unit.Root);

end Rejuvenation.Patterns;
