with String_Sets;    use String_Sets;
with String_Vectors; use String_Vectors;

package String_Sets_Utils is

   function From_Vector (V : Vector) return Set;

   function To_String (S : Set) return String;

end String_Sets_Utils;
