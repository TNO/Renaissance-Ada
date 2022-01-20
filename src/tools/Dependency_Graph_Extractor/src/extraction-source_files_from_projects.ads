with Extraction.Graph_Operations;
with Extraction.Utilities;

private package Extraction.Source_Files_From_Projects is

   procedure Extract_Nodes
     (Project : GPR.Project_Type;
      Graph   : Graph_Operations.Graph_Context);

   procedure Extract_Edges
     (Project          : GPR.Project_Type;
      Context          : Utilities.Project_Context;
      Recurse_Projects : Boolean;
      Graph            : Graph_Operations.Graph_Context);

end Extraction.Source_Files_From_Projects;
