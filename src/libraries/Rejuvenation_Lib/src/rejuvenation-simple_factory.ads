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
   --
   --  Current implementation doesn't work around libadalang limitation:
   --
   --  LIBADALANG.PROJECT_PROVIDER.UNSUPPORTED_VIEW_ERROR:
   --  selected project is aggregate and has more than one sub-project
   --
   --  TODO: Make a workable / reasonable solution!
   --  suggestions / background info:
   --
   --  The situation is more nuanced: even though indeed a given Libadalang
   --  context cannot see whole aggregate projects when they contain
   --  "conflicting" units, it is perfectly possible for a single command to
   --  create one context per aggregated project and run your analysis on all
   --  of them. This specific case is illustrated in our documentation with
   --  an example: see
   --  <https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/
   --   examples/aggregate_projects.html
   --   #creating-one-provide-context-for-each-aggregated-project>.
   --
   --  Note that because of the semantics of aggregate projects, it would
   --  make no sense for a single Libadalang context to see units from all
   --  aggregated projects at once without adding much complexity to the
   --  Libadalang API. For instance, given that multiple units may define the
   --  same type several times, the P_Referenced_Decl property would need to
   --  be modified to return not "THE declaration" referenced by some
   --  expression, but all declarations it may refer to (depending on the
   --  "view" inside the aggregated project). This would be probably a great
   --  burden for both us (Libadalang implementers) and all Libadalang users
   --  (need to deal with a much more complex API even when not dealing with
   --  aggregate projects).


end Rejuvenation.Simple_Factory;
