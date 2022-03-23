with String_Vectors; use String_Vectors;

package String_Vectors_Utils is

   function Join
     (V         : Vector;
      Separator : String := "")
      return String;

end String_Vectors_Utils;
