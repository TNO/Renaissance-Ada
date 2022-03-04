with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Deferred_Constants is

   use type LALCO.Ada_Node_Kind_Type;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node) then
         if Node.Kind = LALCO.Ada_Object_Decl then
            declare
               Object_Decl : constant LAL.Object_Decl := Node.As_Object_Decl;
               Defining_Names : constant LAL.Defining_Name_Array :=
                 Object_Decl.P_Defining_Names;
            begin
               for Defining_Name of Defining_Names loop
                  if not Defining_Name.P_Previous_Part.Is_Null then
                     declare
                        Previous_Name : constant LAL.Defining_Name :=
                          Defining_Name.P_Previous_Part;
                        Previous_Decl : constant LAL.Basic_Decl :=
                          Previous_Name.P_Basic_Decl;
                     begin
                        Graph.Write_Edge
                          (Previous_Name, Previous_Decl, Defining_Name,
                           Object_Decl,
                           Node_Edge_Types.Edge_Type_Is_Implemented_By);
                     end;
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Extract_Edges;

end Extraction.Deferred_Constants;
