with Extraction.Node_Edge_Types;

package body Extraction.Project_Files is

   use type GPR.Project_Type;

   procedure Extract_Nodes
     (Project : GPR.Project_Type; Graph : Graph_Operations.Graph_Context)
   is
   begin
      Graph.Write_Node (Project);
      Graph.Write_Node (Project.Project_Path);
   end Extract_Nodes;

   procedure Extract_Edges
     (Project : GPR.Project_Type; Recurse_Projects : Boolean;
      Graph   : Graph_Operations.Graph_Context)
   is
      Extended_Project : constant GPR.Project_Type := Project.Extended_Project;
   begin
      Graph.Write_Edge
        (Project, Project.Project_Path, Node_Edge_Types.Edge_Type_Source);

      if Recurse_Projects and then Extended_Project /= GPR.No_Project
        and then not Extended_Project.Externally_Built
      then
         Graph.Write_Edge
           (Project, Extended_Project, Node_Edge_Types.Edge_Type_Derives_From);
      end if;

      declare
         Iter : GPR.Project_Iterator :=
           GPR.Start (Project, Recurse_Projects, True);
         Current_Project : GPR.Project_Type;
      begin
         loop
            Current_Project := GPR.Current (Iter);
            exit when Current_Project = GPR.No_Project;

            if Current_Project /= Project
              and then not Current_Project.Externally_Built
            then
               Graph.Write_Edge
                 (Project.Project_Path, Current_Project,
                  Node_Edge_Types.Edge_Type_Imports);
            end if;

            GPR.Next (Iter);
         end loop;
      end;
   end Extract_Edges;

end Extraction.Project_Files;
