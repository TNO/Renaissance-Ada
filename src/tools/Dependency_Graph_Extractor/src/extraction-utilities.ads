with Ada.Containers.Vectors;

private package Extraction.Utilities is

   type Project_Context is private;

   function Open_Project (Project : String) return Project_Context;

   package Analysis_Unit_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => LAL.Analysis_Unit,
      "="        => LAL."=");

   package Project_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => GPR.Project_Type,
      "="        => GPR."=");

   function Open_Analysis_Units
     (Context : Project_Context; Recurse_Projects : Boolean)
      return Analysis_Unit_Vectors.Vector;

   function Get_Projects
     (Context : Project_Context; Recurse_Projects : Boolean)
      return Project_Vectors.Vector;

   function Is_Buildable_File_In_Project_Context
     (Filename : String; Context : Project_Context) return Boolean;

   function Is_Ada_File_In_Project_Context
     (Ada_Filename     : String; Context : Project_Context;
      Recurse_Projects : Boolean) return Boolean;

   function Is_Project_Main_Program
     (Node : LAL.Ada_Node'Class; Context : Project_Context) return Boolean;

   Standard_Unit_File : constant VFS.Virtual_File;

   function Get_Unique_Filename
     (Filename        : String; Directory_Prefix : VFS.Virtual_File;
      Make_Lower_Case : Boolean) return String;
   --  Get a unique name for Filename. The name will be relative to
   --  Directory_Prefix if Directory_Prefix is a parent of Filename. For file
   --  names from the GNATPRO installation path, the installation path will be
   --  replaced by with `%GNATPRO_PATH%`.

   function Get_Unique_Filename
     (File            : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File;
      Make_Lower_Case : Boolean) return String;
   --  Get a unique name for File. The name will be relative to
   --  Directory_Prefix if Directory_Prefix is a parent of File.
   --  For files from the GNATPRO installation path, the installation path
   --  will be replaced by `%GNATPRO_PATH%`.

   function Get_Referenced_Decl (Name : LAL.Name'Class) return LAL.Basic_Decl;

   function Get_Referenced_Defining_Name
     (Name : LAL.Name'Class) return LAL.Defining_Name;

   function Get_Parent_Basic_Decl
     (Node : LAL.Ada_Node'Class) return LAL.Basic_Decl;

   function Is_Relevant_Basic_Decl (Node : LAL.Ada_Node'Class) return Boolean;

private

   use type VFS.Filesystem_String;

   type Project_Context is record
      Project_Environment : GPR.Project_Environment_Access;
      Project_Tree        : GPR.Project_Tree_Access;
      Unit_Provider       : LAL.Unit_Provider_Reference;
      Analysis_Context    : LAL.Analysis_Context;
   end record;

   Standard_Unit_Filename : constant String           := "__standard";
   Standard_Unit_File     : constant VFS.Virtual_File :=
     VFS.Create (+Standard_Unit_Filename, Normalize => True);

end Extraction.Utilities;
