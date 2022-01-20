with Extraction.Graph_Operations;

private package Extraction.Project_Files is

   procedure Extract_Nodes
     (Project : GPR.Project_Type;
      Graph   : Graph_Operations.Graph_Context);

   procedure Extract_Edges
     (Project          : GPR.Project_Type;
      Recurse_Projects : Boolean;
      Graph            : Graph_Operations.Graph_Context);

end Extraction.Project_Files;
