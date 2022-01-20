with Extraction.Graph_Operations;

private package Extraction.Decls is

   procedure Extract_Nodes
     (Node  : LAL.Ada_Node'Class;
      Graph : Graph_Operations.Graph_Context);

   procedure Extract_Edges
     (Node  : LAL.Ada_Node'Class;
      Graph : Graph_Operations.Graph_Context);

end Extraction.Decls;
