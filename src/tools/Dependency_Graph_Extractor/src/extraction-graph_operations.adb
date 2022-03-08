with Extraction.Node_Edge_Types;

package body Extraction.Graph_Operations is

   function Create_Graph_Context
     (Graph_File : access GW.GraphML_File; Directory_Prefix : VFS.Virtual_File;
      Project_Context : Utilities.Project_Context) return Graph_Context
   is
   begin
      return Graph : Graph_Context do
         Graph.Graph_File       := Graph_File;
         Graph.Directory_Prefix := Directory_Prefix;
         Graph.Project_Context  := Project_Context;
      end return;
   end Create_Graph_Context;

   procedure Write_Node (Graph : Graph_Context; File : VFS.Virtual_File) is
      Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (File, Graph.Directory_Prefix);
      Subty : constant GW.Node_Subtype :=
        Node_Edge_Types.Get_File_Subtype (File);
      Node_Attrs : constant GW.Attribute_Value_Sets.Map :=
        Node_Edge_Types.Get_Node_Attributes (File, Graph.Directory_Prefix);
   begin
      if File.Is_Directory then
         Graph.Graph_File.Write_Node
           (Node_Name, Node_Name, Node_Edge_Types.Node_Type_Directory,
            Node_Attrs);
      else
         Graph.Graph_File.Write_Node
           (Node_Name, Node_Name, Node_Edge_Types.Node_Type_File, Subty,
            Node_Attrs);
      end if;
   end Write_Node;

   procedure Write_Node (Graph : Graph_Context; Project : GPR.Project_Type) is
      Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Project, Graph.Directory_Prefix);
      Node_Attrs : constant GW.Attribute_Value_Sets.Map :=
        Node_Edge_Types.Get_Node_Attributes (Project);
   begin
      Graph.Graph_File.Write_Node
        (Node_Name, Node_Name, Node_Edge_Types.Node_Type_Gnat_Project,
         Node_Attrs);
   end Write_Node;

   procedure Write_Node (Graph : Graph_Context; Basic_Decl : LAL.Basic_Decl) is
   begin
      if Basic_Decl.P_Defining_Names'Length = 1 then
         Graph.Write_Node (Basic_Decl.P_Defining_Name, Basic_Decl);
      else
         raise Internal_Extraction_Error
           with "Basic declaration with single defining name expected";
      end if;
   end Write_Node;

   procedure Write_Node
     (Graph      : Graph_Context; Defining_Name : LAL.Defining_Name;
      Basic_Decl : LAL.Basic_Decl)
   is
      Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name
          (Defining_Name, Basic_Decl, Graph.Directory_Prefix);
      Subty : constant GW.Node_Subtype :=
        Node_Edge_Types.Get_Decl_Subtype (Basic_Decl);
      Node_Attrs : constant GW.Attribute_Value_Sets.Map :=
        Node_Edge_Types.Get_Node_Attributes
          (Defining_Name, Basic_Decl, Graph.Project_Context);
   begin
      Graph.Graph_File.Write_Node
        (Node_Name, Node_Name, Node_Edge_Types.Node_Type_Ada_Declaration,
         Subty, Node_Attrs);
   end Write_Node;

   procedure Write_Edge
     (Graph          : Graph_Context; Source_Project : GPR.Project_Type;
      Target_Project : GPR.Project_Type; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs     : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Source_Project, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Target_Project, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Project : GPR.Project_Type;
      Target_File : VFS.Virtual_File; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Source_Project, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Target_File, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph          : Graph_Context; Source_File : VFS.Virtual_File;
      Target_Project : GPR.Project_Type; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs     : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Source_File, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Target_Project, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_File : VFS.Virtual_File;
      Target_File : VFS.Virtual_File; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Source_File, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Target_File, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Unit : LAL.Analysis_Unit;
      Target_Name : LAL.Defining_Name; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Source_Unit, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name
          (Target_Name, Target_Decl, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Unit : LAL.Analysis_Unit;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name
          (Source_Name, Source_Decl, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name (Target_Unit, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Decl : LAL.Basic_Decl'Class;
      Target_Decl : LAL.Basic_Decl'Class; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
   begin
      if Source_Decl.P_Defining_Names'Length = 1 then
         Graph.Write_Edge
           (Source_Decl.P_Defining_Name, Source_Decl, Target_Decl, Edge_Ty,
            Edge_Attrs);
      else
         raise Internal_Extraction_Error
           with "Source declaration with single defining name expected";
      end if;
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
   begin
      if Target_Decl.P_Defining_Names'Length = 1 then
         Graph.Write_Edge
           (Source_Name, Source_Decl, Target_Decl.P_Defining_Name, Target_Decl,
            Edge_Ty, Edge_Attrs);
      else
         raise Internal_Extraction_Error
           with "Target declaration with single defining name expected";
      end if;
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Decl : LAL.Basic_Decl'Class;
      Target_Name : LAL.Defining_Name; Target_Decl : LAL.Basic_Decl'Class;
      Edge_Ty     : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
   begin
      if Source_Decl.P_Defining_Names'Length = 1 then
         Graph.Write_Edge
           (Source_Decl.P_Defining_Name, Source_Decl, Target_Name, Target_Decl,
            Edge_Ty, Edge_Attrs);
      else
         raise Internal_Extraction_Error
           with "Source declaration with single defining name expected";
      end if;
   end Write_Edge;

   procedure Write_Edge
     (Graph       : Graph_Context; Source_Name : LAL.Defining_Name;
      Source_Decl : LAL.Basic_Decl'Class; Target_Name : LAL.Defining_Name;
      Target_Decl : LAL.Basic_Decl'Class; Edge_Ty : GraphML_Writers.Edge_Type;
      Edge_Attrs  : GW.Attribute_Value_Sets.Map :=
        GW.Attribute_Value_Sets.Empty)
   is
      Source_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name
          (Source_Name, Source_Decl, Graph.Directory_Prefix);
      Target_Node_Name : constant String :=
        Node_Edge_Types.Get_Node_Name
          (Target_Name, Target_Decl, Graph.Directory_Prefix);
   begin
      Graph.Graph_File.Write_Edge
        (Source_Node_Name, Target_Node_Name, Edge_Ty, Edge_Attrs);
   end Write_Edge;

end Extraction.Graph_Operations;
