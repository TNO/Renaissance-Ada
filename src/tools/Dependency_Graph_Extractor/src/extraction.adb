with Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;
with Extraction.Bodies_For_Decls;
with Extraction.Bodies_For_Entries;
with Extraction.Decls;
with Extraction.Deferred_Constants;
with Extraction.Derived_Type_Defs;
with Extraction.Direct_Calls;
with Extraction.File_System;
with Extraction.Generic_Instantiations;
with Extraction.Graph_Operations;
with Extraction.Node_Edge_Types;
with Extraction.Primitive_Subps;
with Extraction.Project_Files;
with Extraction.References_Of_Decls;
with Extraction.Renamings;
with Extraction.Source_Files_From_Projects;
with Extraction.Subp_Overrides;
with Extraction.Decl_Types;
with Extraction.Utilities;
with Extraction.With_Clauses;

package body Extraction is

   --  TODO: Improve node naming such that line numbers are not needed
   --  TODO: Relate arguments in generic instantiations with formal parameters
   --        of generics

   use type VFS.Filesystem_String;
   use type VFS.Virtual_File;

   function Node_Attributes return GW.Attribute_Definition_Sets.Map renames
     Node_Edge_Types.Node_Attributes;

   function Edge_Attributes return GW.Attribute_Definition_Sets.Map renames
     Node_Edge_Types.Edge_Attributes;

   procedure Extract_Dependency_Graph
     (Project_Filename : String; Recurse_Projects : Boolean;
      Directory_Prefix : VFS.Virtual_File; Graph_File : in out GW.GraphML_File)
   is
      Context : constant Utilities.Project_Context :=
        Utilities.Open_Project (Project_Filename);
      Projects : constant Utilities.Project_Vectors.Vector :=
        Utilities.Get_Projects (Context, Recurse_Projects);
      Units : constant Utilities.Analysis_Unit_Vectors.Vector :=
        Utilities.Open_Analysis_Units (Context, Recurse_Projects);
      Graph : constant Graph_Operations.Graph_Context :=
        Graph_Operations.Create_Graph_Context
          (Graph_File'Unchecked_Access, Directory_Prefix, Context);

      procedure Handle_Exception
        (E : Ada.Exceptions.Exception_Occurrence; Node : LAL.Ada_Node'Class);
      procedure Handle_Exception
        (E : Ada.Exceptions.Exception_Occurrence; Node : LAL.Ada_Node'Class)
      is
         Message : constant String := Ada.Exceptions.Exception_Message (E);
      begin
         if not Ada.Strings.Equal_Case_Insensitive (Message, "memoized error")
         then
            Ada.Text_IO.Put_Line
              ("Encountered Libadalang problem: " & Message);
            Node.Print;
         end if;
      end Handle_Exception;

      function Node_Visitor
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
      function Node_Visitor
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         --  TODO: Remove exception block once all libadalang problems
         --        have been fixed.
         begin
            Decls.Extract_Nodes (Node, Graph);
            Subp_Overrides.Extract_Nodes (Node, Graph);
            Primitive_Subps.Extract_Nodes (Node, Graph);
            References_Of_Decls.Extract_Nodes (Node, Graph);
         exception
            when E : LALCO.Property_Error =>
               Handle_Exception (E, Node);
         end;

         return LALCO.Into;
      end Node_Visitor;

      function Edge_Visitor
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
      function Edge_Visitor
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         --  TODO: Remove exception block once all libadalang problems
         --        have been fixed.
         begin
            Decls.Extract_Edges (Node, Graph);
            Subp_Overrides.Extract_Edges (Node, Graph);
            Primitive_Subps.Extract_Edges (Node, Graph);
            Generic_Instantiations.Extract_Edges (Node, Graph);
            References_Of_Decls.Extract_Edges (Node, Graph);
            Bodies_For_Decls.Extract_Edges (Node, Graph);
            Bodies_For_Entries.Extract_Edges (Node, Graph);
            Renamings.Extract_Edges (Node, Graph);
            Direct_Calls.Extract_Edges (Node, Graph);
            With_Clauses.Extract_Edges (Node, Graph);
            Derived_Type_Defs.Extract_Edges (Node, Graph);
            Decl_Types.Extract_Edges (Node, Graph);
            Deferred_Constants.Extract_Edges (Node, Graph);
         exception
            when E : LALCO.Property_Error =>
               Handle_Exception (E, Node);
         end;

         return LALCO.Into;
      end Edge_Visitor;

   begin
      --  Node extraction

      --  File_System.Extract_Nodes should occur before the extraction of any
      --  other kind of node (see also the implementation of
      --  Get_Node_Attributes in Extraction.Node_Edge_Types).
      if Directory_Prefix /= VFS.No_File then
         File_System.Extract_Nodes (Directory_Prefix, Graph);
      end if;

      for Project of Projects loop
         Ada.Text_IO.Put_Line
           ("-- " & (+Project.Project_Path.Full_Name) & " --");
         Project_Files.Extract_Nodes (Project, Graph);
         Source_Files_From_Projects.Extract_Nodes (Project, Graph);
      end loop;

      for Unit of Units loop
         Ada.Text_IO.Put_Line ("-- " & Unit.Get_Filename & " --");
         Unit.Root.Traverse (Node_Visitor'Access);
      end loop;

      --  Edge extraction.

      if Directory_Prefix /= VFS.No_File then
         File_System.Extract_Edges (Directory_Prefix, Graph);
      end if;

      for Project of Projects loop
         Ada.Text_IO.Put_Line
           ("== " & (+Project.Project_Path.Full_Name) & " ==");
         Project_Files.Extract_Edges (Project, Recurse_Projects, Graph);
         Source_Files_From_Projects.Extract_Edges
           (Project, Context, Recurse_Projects, Graph);
      end loop;

      for Unit of Units loop
         Ada.Text_IO.Put_Line ("== " & Unit.Get_Filename & " ==");
         Unit.Root.Traverse (Edge_Visitor'Access);
      end loop;
   end Extract_Dependency_Graph;

end Extraction;
