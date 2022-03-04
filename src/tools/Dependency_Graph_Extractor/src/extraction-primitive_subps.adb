with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Primitive_Subps is

   use type LAL.Analysis_Unit;
   use type LALCO.Ada_Node_Kind_Type;

   procedure Extract_Nodes
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node)
        and then Node.Kind = LALCO.Ada_Type_Decl
      then
         declare
            Type_Decl  : constant LAL.Type_Decl        := Node.As_Type_Decl;
            Primitives : constant LAL.Basic_Decl_Array :=
              Type_Decl.P_Get_Primitives;
         begin
            --  Add non-standard subprograms that are primitive
            --  even if external.
            for Basic_Decl of Primitives loop
               if Basic_Decl.Unit /= Basic_Decl.P_Standard_Unit then
                  Graph.Write_Node (Basic_Decl);
               end if;
            end loop;
         end;
      end if;
   end Extract_Nodes;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node)
        and then Node.Kind = LALCO.Ada_Type_Decl
      then
         declare
            Type_Decl  : constant LAL.Type_Decl        := Node.As_Type_Decl;
            Primitives : constant LAL.Basic_Decl_Array :=
              Type_Decl.P_Get_Primitives;
         begin
            for Basic_Decl of Primitives loop
               if Basic_Decl.Unit /= Basic_Decl.P_Standard_Unit then
                  Graph.Write_Edge
                    (Basic_Decl, Type_Decl,
                     Node_Edge_Types.Edge_Type_Is_Primitive_Subprogram_Of);
               end if;
            end loop;
         end;
      end if;
   end Extract_Edges;

end Extraction.Primitive_Subps;
