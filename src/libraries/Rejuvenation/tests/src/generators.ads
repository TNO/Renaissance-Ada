with Libadalang.Analysis;                use Libadalang.Analysis;
with Libadalang.Common;                  use Libadalang.Common;
with String_Vectors;                     use String_Vectors;
private with Ada.Strings.Unbounded;

package Generators is
   --  For now: single placeholder - so single Name

   --  package String_Vector_Vectors is new Ada.Containers.Indefinite_Vectors
   --    (Index_Type => Positive,
   --     Element_Type => String_Vectors.Vector);

   type Generator is private;
   function Generate_Pattern (G : Generator) return Analysis_Unit;
   function Generate_Instance (G : Generator) return Analysis_Unit;

   function Get_Name (G : Generator) return String;

   function Get_Values (G : Generator) return String_Vectors.Vector;

   type String_Generator is
   not null access function (Strings : String_Vectors.Vector) return String;

   function Make_Generator (Name : String;
                            Values : String_Vectors.Vector;
                            Rule : Grammar_Rule;
                            SG : String_Generator
                           ) return Generator;

private
   use Ada.Strings.Unbounded;

   type Generator is record
      Name : Unbounded_String;
      Values : String_Vectors.Vector;
      --  TODO: is a copy needed to make it immutable from the outside?
      Rule : Grammar_Rule;
      SG : String_Generator;
   end record;

   function Make_Generator
     (Name : String; Values : String_Vectors.Vector; Rule : Grammar_Rule;
      SG : String_Generator)
      return Generator
   is
     ((To_Unbounded_String (Name), Values, Rule, SG));

   function Get_Name (G : Generator) return String
   is
     (To_String (G.Name));

   function Get_Values (G : Generator) return String_Vectors.Vector
   is
     (G.Values);

end Generators;
