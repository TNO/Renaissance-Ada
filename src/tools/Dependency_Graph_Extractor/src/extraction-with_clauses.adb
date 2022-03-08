with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.With_Clauses is

   use type LALCO.Ada_Node_Kind_Type;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Node.Kind = LALCO.Ada_With_Clause then
         declare
            With_Clause : constant LAL.With_Clause := Node.As_With_Clause;
         begin
            for Package_Name of With_Clause.F_Packages loop
               if not Utilities.Get_Referenced_Decl (Package_Name).Is_Null
               then -- TODO: Remove after TB01-005 has been fixed
                  declare
                     Target_Name : constant LAL.Defining_Name :=
                       Utilities.Get_Referenced_Defining_Name (Package_Name);
                     Target_Decl : constant LAL.Basic_Decl :=
                       Utilities.Get_Referenced_Decl (Package_Name);
                  begin
                     Graph.Write_Edge
                       (Package_Name.Unit, Target_Name, Target_Decl,
                        Node_Edge_Types.Edge_Type_Imports);
                  end;
               end if;
            end loop;
         end;
      end if;
   end Extract_Edges;

end Extraction.With_Clauses;
