with Extraction.Utilities;

private package Extraction.Graph_Operations is

   type Graph_Context is tagged private;

   function Create_Graph_Context
     (Graph_File : access GW.GraphML_File; Directory_Prefix : VFS.Virtual_File;
      Project_Context : Utilities.Project_Context) return Graph_Context;

   procedure Write_Node (Graph : Graph_Context; File : VFS.Virtual_File);

   procedure Write_Node (Graph : Graph_Context; Project : GPR.Project_Type);

   procedure Write_Node (Graph : Graph_Context; Basic_Decl : LAL.Basic_Decl);

   procedure Write_Node
     (Graph      : Graph_Context; Defining_Name : LAL.Defining_Name;
      Basic_Decl : LAL.Basic_Decl);

   procedure Write_Edge
     (Graph          : Graph_Context; Source_Project : GPR.Project_Type;
      Target_Project : GPR.Project_Type; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs     : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Project : GPR.Project_Type;
      Target_File : VFS.Virtual_File; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph          : Graph_Context; Source_File : VFS.Virtual_File;
      Target_Project : GPR.Project_Type; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs     : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_File : VFS.Virtual_File;
      Target_File : VFS.Virtual_File; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Unit : LAL.Analysis_Unit;
      Target_Name : LAL.Defining_Name; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Unit : LAL.Analysis_Unit;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Decl : LAL.Basic_Decl'Class;
      Target_Decl : LAL.Basic_Decl'Class; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Decl : LAL.Basic_Decl'Class;
      Target_Name : LAL.Defining_Name; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Name : LAL.Defining_Name;
      Target_Decl : LAL.Basic_Decl'Class; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty);

private

   type Graph_Context is tagged record
      Graph_File       : access GW.GraphML_File;
      Directory_Prefix : VFS.Virtual_File;
      Project_Context  : Utilities.Project_Context;
   end record;

end Extraction.Graph_Operations;
