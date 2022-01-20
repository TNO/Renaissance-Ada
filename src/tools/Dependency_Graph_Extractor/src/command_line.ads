with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Command_Line is

   package SU renames Ada.Strings.Unbounded;

   package Input_File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => SU.Unbounded_String,
      "="          => SU."=");

   function Parse_Command_Line
     (Input_Files      : out Input_File_Vectors.Vector;
      Recurse_Projects : out Boolean;
      Directory_Prefix : out SU.Unbounded_String;
      Output_File      : out SU.Unbounded_String)
      return Boolean;

end Command_Line;
