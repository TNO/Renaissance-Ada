with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Subp_Overrides is

   procedure Extract_Nodes
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node)
        and then Node.As_Basic_Decl.P_Is_Subprogram
      then
         declare
            Decl       : constant LAL.Basic_Decl       := Node.As_Basic_Decl;
            Base_Decls : constant LAL.Basic_Decl_Array :=
              Decl.P_Base_Subp_Declarations;
         begin
            --  Add all subprograms that are overriden even if external.
            for Base_Decl of Base_Decls loop
               Graph.Write_Node (Base_Decl);
            end loop;
         end;
      end if;
   end Extract_Nodes;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node)
        and then Node.As_Basic_Decl.P_Is_Subprogram
      then
         declare
            Decl       : constant LAL.Basic_Decl       := Node.As_Basic_Decl;
            Base_Decls : constant LAL.Basic_Decl_Array :=
              Decl.P_Base_Subp_Declarations;
         begin
            if (for some Base_Decl of Base_Decls => Base_Decl = Decl)
            then --  Ignore implementations later in the same file.
               for Base_Decl of Base_Decls loop
                  if Base_Decl /= Decl
                  then
                     --  Ignore Decl itself, which will be in the set,
                     --  unless it is an implementation later in the same file.
                     Graph.Write_Edge
                       (Base_Decl, Decl,
                        Node_Edge_Types.Edge_Type_Is_Overridden_By);
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Extract_Edges;

end Extraction.Subp_Overrides;
