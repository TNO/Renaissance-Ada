with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Bodies_For_Decls is

   use type LALCO.Ada_Node_Kind_Type;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node) then
         if Node.Kind in LALCO.Ada_Subp_Decl | LALCO.Ada_Generic_Subp_Decl |
               LALCO.Ada_Subp_Body_Stub | LALCO.Ada_Package_Decl |
               LALCO.Ada_Generic_Package_Decl | LALCO.Ada_Package_Body_Stub |
               LALCO.Ada_Task_Type_Decl | LALCO.Ada_Task_Body_Stub |
               LALCO.Ada_Single_Task_Decl | LALCO.Ada_Protected_Type_Decl |
               LALCO.Ada_Protected_Body_Stub   |
               LALCO.Ada_Single_Protected_Decl |
               LALCO.Ada_Incomplete_Type_Decl  |
               LALCO.Ada_Incomplete_Tagged_Type_Decl
         then
            if not Node.As_Basic_Decl.P_Next_Part_For_Decl.Is_Null
            then -- Ignore externals and non-existing bodies.
               declare
                  Decl      : constant LAL.Basic_Decl := Node.As_Basic_Decl;
                  Body_Part : constant LAL.Basic_Decl :=
                    Decl.P_Next_Part_For_Decl;
               begin
                  Graph.Write_Edge
                    (Decl, Body_Part,
                     Node_Edge_Types.Edge_Type_Is_Implemented_By);
               end;
            end if;
         end if;
      end if;
   end Extract_Edges;

end Extraction.Bodies_For_Decls;
