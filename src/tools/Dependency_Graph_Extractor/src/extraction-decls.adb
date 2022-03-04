with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Decls is

   use type LALCO.Ada_Node_Kind_Type;

   function Is_Standard_Package_Decl
     (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Standard_Package_Decl (Node : LAL.Ada_Node'Class) return Boolean
   is
      Standard_Unit : constant LAL.Compilation_Unit :=
        Node.P_Standard_Unit.Root.As_Compilation_Unit;
      Standard_Pkg_Decl : constant LAL.Basic_Decl :=
        Standard_Unit.F_Body.As_Library_Item.F_Item;
   begin
      return
        Node.Kind = LALCO.Ada_Package_Decl and then Node = Standard_Pkg_Decl;
   end Is_Standard_Package_Decl;

   procedure Extract_Nodes
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node) then
         declare
            Basic_Decl : constant LAL.Basic_Decl := Node.As_Basic_Decl;
         begin
            for Defining_Name of Basic_Decl.P_Defining_Names loop
               Graph.Write_Node (Defining_Name, Basic_Decl);
            end loop;
         end;
      end if;
   end Extract_Nodes;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node)
        and then not Is_Standard_Package_Decl (Node)
      then
         declare
            Basic_Decl : constant LAL.Basic_Decl := Node.As_Basic_Decl;
            Parent     : constant LAL.Basic_Decl :=
              Utilities.Get_Parent_Basic_Decl (Basic_Decl);
         begin
            for Defining_Name of Basic_Decl.P_Defining_Names loop
               Graph.Write_Edge
                 (Defining_Name, Basic_Decl, Basic_Decl.Unit,
                  Node_Edge_Types.Edge_Type_Source);

               if not Basic_Decl.P_Is_Compilation_Unit_Root
                 or else Basic_Decl.Parent.Kind = LALCO.Ada_Subunit
               then
                  Graph.Write_Edge
                    (Parent, Defining_Name, Basic_Decl,
                     Node_Edge_Types.Edge_Type_Contains);
               elsif Parent.P_Body_Part_For_Decl /= Basic_Decl
                 and then not Is_Standard_Package_Decl (Parent)
               then
                  Graph.Write_Edge
                    (Parent, Defining_Name, Basic_Decl,
                     Node_Edge_Types.Edge_Type_Is_Parent_Of);
               end if;
            end loop;
         end;
      end if;
   end Extract_Edges;

end Extraction.Decls;
