with Ada.Containers.Indefinite_Ordered_Maps;

package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String, Element_Type => String);
