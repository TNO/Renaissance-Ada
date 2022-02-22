with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Generator.Match_Pattern_Specific is
   use Ada.Containers;

   package List_String is new Vectors (Positive, Unbounded_String);

   function Equivalent_Key (Left, Right : String) return Boolean;
   function Equivalent_Value (Left, Right : List_String.Vector) return Boolean;

   package Mapping_Single_Map is new Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => List_String.Vector,
      Hash     => Ada.Strings.Hash, Equivalent_Keys => Equivalent_Key,
      "="      => Equivalent_Value);

   procedure Append
     (Map : in out Mapping_Single_Map.Map; Key : String; Value : String);
   procedure Process_Node_Kinds;
   procedure Process_Type_Decl;
   procedure Process_Subp;
   procedure Generate_Match_Specific;

   procedure Main;

end Generator.Match_Pattern_Specific;
