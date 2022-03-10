with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Command_Line is

   package S_U renames Ada.Strings.Unbounded;

   package Input_File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => S_U.Unbounded_String,
      "="          => S_U."=");

   function Parse_Command_Line
     (Input_Files      : out Input_File_Vectors.Vector;
      Recurse_Projects : out Boolean;
      Directory_Prefix : out S_U.Unbounded_String;
      Output_File      : out S_U.Unbounded_String)
      return Boolean;

end Command_Line;
