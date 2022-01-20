with Extraction.Graph_Operations;

private package Extraction.File_System is

   procedure Extract_Nodes
     (Directory : VFS.Virtual_File;
      Graph     : Graph_Operations.Graph_Context);

   procedure Extract_Edges
     (Directory : VFS.Virtual_File;
      Graph     : Graph_Operations.Graph_Context);

end Extraction.File_System;
