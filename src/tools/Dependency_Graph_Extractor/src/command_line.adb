with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

package body Command_Line is

   function Parse_Command_Line
     (Input_Files      : out Input_File_Vectors.Vector;
      Recurse_Projects : out Boolean;
      Directory_Prefix : out S_U.Unbounded_String;
      Output_File      : out S_U.Unbounded_String) return Boolean
   is
      Config             : Command_Line_Configuration;
      Output             : aliased String_Access := null;
      No_Project_Recurse : aliased Boolean;
      Prefix             : aliased String_Access := null;
   begin
      Initialize_Option_Scan;

      Define_Switch
        (Config, Switch => "-h", Long_Switch => "--help",
         Help           => "Display help");
      Define_Switch
        (Config, Output'Access, Switch => "-o:", Long_Switch => "--output=",
         Help                          => "The output GraphML file");
      Define_Switch
        (Config, No_Project_Recurse'Access, Switch => "",
         Long_Switch                               => "--no-project-recurse",
         Help => "Do not recurse projects");
      Define_Switch
        (Config, Prefix'Access, Switch => "-p:", Long_Switch => "--prefix=",
         Help                          =>
           "Directory prefix stripped from the file names" &
           " in the output graph");

      Getopt (Config);

      Output_File :=
        (if Output.all'Length = 0 then S_U.Null_Unbounded_String
         else S_U.To_Unbounded_String (Output.all));
      Free (Output);
      Directory_Prefix :=
        (if Prefix.all'Length = 0 then S_U.Null_Unbounded_String
         else S_U.To_Unbounded_String (Prefix.all));
      Free (Prefix);
      Recurse_Projects := not No_Project_Recurse;

      Input_File_Vectors.Clear (Input_Files);

      loop
         declare
            Input : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when Input'Length = 0;
            Input_Files.Append (S_U.To_Unbounded_String (Input));
         end;
      end loop;

      return True;
   exception
      when others =>
         return False;
   end Parse_Command_Line;

end Command_Line;
