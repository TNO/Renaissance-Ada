with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package Rejuvenation.Simple_Factory is

   package Analysis_Units is new Ada.Containers.Vectors
     (Positive, Analysis_Unit);
   package Unbounded_Strings is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   Parse_Exception : exception;

   procedure Put_Diagnostics (File : File_Type; Unit : Analysis_Unit) with
      Pre => Is_Open (File) and then Ada.Text_IO.Mode (File) /= In_File;

   procedure Put_Diagnostics (Unit : Analysis_Unit);

   function Get_Ada_Source_Files_From_Project
     (Project_Filename : String; Recursive : Boolean := True)
      return Unbounded_Strings.Vector;
   --  Return the file paths of all Ada files in the given project
   --  When the project contains subprojects,
   --  Ada files in those subprojects will be include based on the
   --  value of the Recursive flag
   --  Pre: Project_Filename points to an Ada project file

   function Get_Ada_Source_Files_From_Directory
     (Directory_Name : String; Recursive : Boolean := True)
      return Unbounded_Strings.Vector;
   --  Return the file paths of all Ada files in the given directory
   --  When the directory contains subdirectories,
   --  Ada files in those subdirectories will be include based on the
   --  value of the Recursive flag
   --  Pre: Directory_Name points to an Ada project file

   function Analyze_Fragment
     (Fragment : String; Rule : Grammar_Rule := Default_Grammar_Rule)
      return Analysis_Unit;
   --  Return the analysis unit of the given fragment of Ada code
   --  Optionally a specific grammar rule can be specified.
   --  Raises an exception if parsing fails.

   function Analyze_File (Filename : String) return Analysis_Unit;
   --  Return the analysis unit of the given Ada file
   --  Pre: Filename points to an Ada file

   function Analyze_File_In_Project
     (Filename : String; Project_Filename : String) return Analysis_Unit;
   --  Return the analysis unit of the given Ada file in the context
   --  of the given project
   --  Pre: Filename points to an Ada file
   --  Pre: Project_Filename points to an Ada project file

   function Analyze_Project
     (Project_Filename : String; Recursive : Boolean := True)
      return Analysis_Units.Vector;
   --  Return the analysis units of all Ada files in the given project
   --  When the project contains subprojects,
   --  Ada files in those subprojects will be include based on the
   --  value of the Recursive flag
   --  Pre: Project_Filename points to an Ada project file

end Rejuvenation.Simple_Factory;
