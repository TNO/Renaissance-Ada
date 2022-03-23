with Ada.Containers.Vectors;
with GNATCOLL.Projects;

package Rejuvenation.Factory is

   --  This Factory is for backwards compatibility / advanced usage only
   --  Please use Rejuvenation.Simple_Factory

   --  Methods to create AST instances from fragments of Ada source code.
   --
   --  The return type of these methods is Analysis_Unit; the AST node can
   --  be obtained using   .Root   .
   --
   --  The return type is Analysis_Unit, because AST nodes are only valid as
   --  long as the corresponding Analysis_Unit is in scope.

   -- Project methods --------

   type Project_Context is private;
   --  A Project_Context that can be used to open files within a GPR project.

   function Create_Context return Project_Context;
--  Create an empty Project_Context, not associated with any GPR project file.
   function Open_Project (Project_Path : String) return Project_Context;
   --  Return a Project_Context for reference resolving within a GPR project.

   -- Core methods --------

   Parse_Exception : exception;
   --  Exception that indicates that a string could not be parsed.

   function Open_File
     (Filename : String; Context : Project_Context := Create_Context)
      return Analysis_Unit;
   --  Return AST for the given Ada file.
   --  Optionally a project-specific Analysis_Context can be specified.

   package Analysis_Unit_Vectors is new Ada.Containers.Vectors
     (Positive, Analysis_Unit);
   --  Data type for list of analysis units.

   function Open_Files_From_Project
     (Context : Project_Context; Recursive : Boolean := True)
      return Analysis_Unit_Vectors.Vector;
   --  Return ASTs for all Ada files in a GPR project and
   --  recursively include projects not built separately.

   function Is_Ada_File_Built_By_Project
     (Filename  : String; Context : Project_Context;
      Recursive : Boolean := True) return Boolean;
   --  Check whether a file is an Ada file built by a project.

   function Is_Project_Main_Program
     (Node : Ada_Node'Class; Context : Project_Context) return Boolean;
   --  Check whether a node is a main program of a project.

private

   type Project_Context is record
      Environment  : GNATCOLL.Projects.Project_Environment_Access := null;
      Project_Tree : GNATCOLL.Projects.Project_Tree_Access        :=
        new GNATCOLL.Projects.Project_Tree;
      Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Context  : Analysis_Context        := No_Analysis_Context;
   end record;

end Rejuvenation.Factory;
