with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

package body Command_Line is

   function Parse_Command_Line
     (Input_Files      : out Input_File_Vectors.Vector;
      Recurse_Projects : out Boolean;
      Directory_Prefix : out SU.Unbounded_String;
      Output_File      : out SU.Unbounded_String)
      return Boolean
   is
      Config             : Command_Line_Configuration;
      Output             : aliased String_Access := null;
      No_Project_Recurse : aliased Boolean;
      Prefix             : aliased String_Access := null;
   begin
      Initialize_Option_Scan;

      Define_Switch(Config, "-h", "--help", "Display help");
      Define_Switch(Config, Output'Access, "-o:", "--output=", "The output GraphML file");
      Define_Switch(Config, No_Project_Recurse'Access, "", "--no-project-recurse", "Do not recurse projects");
      Define_Switch(Config, Prefix'Access, "-p:", "--prefix=", "Directory prefix stripped from the file names in the output graph");

      Getopt(Config);

      Output_File := (if Output.all'Length = 0 then SU.Null_Unbounded_String else SU.To_Unbounded_String(Output.all));
      Free(Output);
      Directory_Prefix := (if Prefix.all'Length = 0 then SU.Null_Unbounded_String else SU.To_Unbounded_String(Prefix.all));
      Free(Prefix);
      Recurse_Projects := not No_Project_Recurse;

      Input_File_Vectors.Clear(Input_Files);

      loop
         declare
            Input : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when Input'Length = 0;
            Input_Files.Append(SU.To_Unbounded_String(Input));
         end;
      end loop;

      return True;
   exception
      when others =>
         return False;
   end Parse_Command_Line;

end Command_Line;
