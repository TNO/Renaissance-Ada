with Extraction.Utilities;

private package Extraction.Node_Edge_Types is

   --  Node names.

   function Get_Node_Name
     (File : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File)
      return String;

   function Get_Node_Name
     (Project : GPR.Project_Type; Directory_Prefix : VFS.Virtual_File)
      return String;

   function Get_Node_Name
     (Analysis_Unit : LAL.Analysis_Unit; Directory_Prefix : VFS.Virtual_File)
      return String;

   function Get_Node_Name
     (Defining_Name    : LAL.Defining_Name; Basic_Decl : LAL.Basic_Decl'Class;
      Directory_Prefix : VFS.Virtual_File) return String;

   --  Node attributes.

   Node_Attribute_Fully_Qualified_Name : constant SU.Unbounded_String :=
     +"fullyQualifiedName";
   Node_Attribute_Is_Formal_Parameter : constant SU.Unbounded_String :=
     +"isFormalParameter";
   Node_Attribute_Is_Main_Program : constant SU.Unbounded_String :=
     +"isMainProgram";
   Node_Attribute_Relative_Name : constant SU.Unbounded_String :=
     +"relativeName";
   Node_Attribute_Source_Location : constant SU.Unbounded_String :=
     +"sourceLocation";

   function Node_Attributes return GW.Attribute_Definition_Sets.Map;

   function Get_Node_Attributes
     (File : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File)
      return GW.Attribute_Value_Sets.Map;

   function Get_Node_Attributes
     (Project : GPR.Project_Type) return GW.Attribute_Value_Sets.Map;

   function Get_Node_Attributes
     (Defining_Name : LAL.Defining_Name; Basic_Decl : LAL.Basic_Decl;
      Context : Utilities.Project_Context) return GW.Attribute_Value_Sets.Map;

   --  Node types.

   --  File and directory types.
   Node_Type_File      : constant GW.Node_Type := "File";
   Node_Type_Directory : constant GW.Node_Type := "Directory";

   --  File subtypes.
   Node_Type_Unknown_File_Type      : constant GW.Node_Subtype := "";
   Node_Type_Ada_Body_File : constant GW.Node_Subtype := "AdaBodyFile";
   Node_Type_Ada_Specification_File : constant GW.Node_Subtype :=
     "AdaSpecificationFile";
   Node_Type_C_Header_File : constant GW.Node_Subtype :=
     "CHeaderFile"; -- Contained in projects, but not analyzed.
   Node_Type_C_Source_File : constant GW.Node_Subtype :=
     "CSourceFile"; -- Contained in projects, but not analyzed.
   Node_Type_Gnat_Project_File : constant GW.Node_Subtype := "GnatProjectFile";

   --  Project types.
   Node_Type_Gnat_Project : constant GW.Node_Type := "GnatProject";

   --  Declarations.
   Node_Type_Ada_Declaration : constant GW.Node_Type := "AdaDeclaration";

   --  Declaration subtypes.
   Node_Type_Ada_Component_Declaration : constant GW.Node_Subtype :=
     "AdaComponentDeclaration";
   Node_Type_Ada_Discriminant_Declaration : constant GW.Node_Subtype :=
     "AdaDiscriminantDeclaration";
   Node_Type_Ada_Entry_Declaration : constant GW.Node_Subtype :=
     "AdaEntryDeclaration";
   Node_Type_Ada_Enum_Literal_Declaration : constant GW.Node_Subtype :=
     "AdaEnumLiteralDeclaration";
   Node_Type_Ada_Exception_Declaration : constant GW.Node_Subtype :=
     "AdaExceptionDeclaration";
   Node_Type_Ada_Number_Declaration : constant GW.Node_Subtype :=
     "AdaNumberDeclaration";
   Node_Type_Ada_Object_Declaration : constant GW.Node_Subtype :=
     "AdaObjectDeclaration";
   Node_Type_Ada_Package_Declaration : constant GW.Node_Subtype :=
     "AdaPackageDeclaration";
   Node_Type_Ada_Protected_Declaration : constant GW.Node_Subtype :=
     "AdaProtectedDeclaration";
   Node_Type_Ada_Subprogram_Declaration : constant GW.Node_Subtype :=
     "AdaSubprogramDeclaration";
   Node_Type_Ada_Task_Declaration : constant GW.Node_Subtype :=
     "AdaTaskDeclaration";
   Node_Type_Ada_Type_Declaration : constant GW.Node_Subtype :=
     "AdaTypeDeclaration";

   function Get_File_Subtype (File : VFS.Virtual_File) return GW.Node_Subtype;

   function Get_Decl_Subtype (Decl : LAL.Basic_Decl) return GW.Node_Subtype;

   --  Edge attributes.

   Edge_Attribute_Has_Access_Type : constant SU.Unbounded_String :=
     +"hasAccessType";
   Edge_Attribute_Has_Array_Type : constant SU.Unbounded_String :=
     +"hasArrayType";
   Edge_Attribute_Is_Dispatching : constant SU.Unbounded_String :=
     +"isDispatching";

   function Edge_Attributes return GW.Attribute_Definition_Sets.Map;

   function Get_Edge_Attributes
     (Expr : LAL.Expr'Class) return GW.Attribute_Value_Sets.Map;

   function Get_Edge_Attributes
     (Type_Expr : LAL.Type_Expr'Class; Can_Have_Array_Type : Boolean)
      return GW.Attribute_Value_Sets.Map;

   --  Edge types.

   --  Declaration-related and project-related.
   Edge_Type_Contains : constant GW.Edge_Type := "Contains";
   --  For declaration nesting and file in project/directory containment.
   Edge_Type_Derives_From : constant GW.Edge_Type := "DerivesFrom";
   --  For type derivations and project extensions.
   Edge_Type_Imports : constant GW.Edge_Type := "Imports";
   --  For `with` clauses and project imports.
   Edge_Type_Source : constant GW.Edge_Type := "Source";
   --  Source of a declaration or project.

   --   Project-related.
   Edge_Type_Compiles : constant GW.Edge_Type := "Compiles";

   --  Declaration-related.
   Edge_Type_Instantiates      : constant GW.Edge_Type := "Instantiates";
   Edge_Type_Is_Implemented_By : constant GW.Edge_Type := "IsImplementedBy";
   Edge_Type_Is_Overridden_By  : constant GW.Edge_Type := "IsOverriddenBy";
   Edge_Type_Is_Parent_Of      : constant GW.Edge_Type := "IsParentOf";
   --  For child packages and their parents.
   Edge_Type_Is_Primitive_Subprogram_Of : constant GW.Edge_Type :=
     "IsPrimitiveSubprogramOf";
   Edge_Type_References : constant GW.Edge_Type := "References";
   Edge_Type_Renames    : constant GW.Edge_Type := "Renames";

   --  Call-related.
   Edge_Type_Calls : constant GW.Edge_Type := "Calls";

   --  Type-related.
   Edge_Type_Has_Parameter_Of_Type : constant GW.Edge_Type :=
     "HasParameterOfType";
   Edge_Type_Has_Return_Of_Type : constant GW.Edge_Type := "HasReturnOfType";
   Edge_Type_Has_Type           : constant GW.Edge_Type := "HasType";

end Extraction.Node_Edge_Types;
