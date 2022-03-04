with Extraction.Node_Edge_Types;

package body Extraction.Source_Files_From_Projects is

   use type VFS.File_Array_Access;
   use type VFS.Filesystem_String;

   procedure Extract_Nodes
     (Project : GPR.Project_Type; Graph : Graph_Operations.Graph_Context)
   is
      Source_Files : VFS.File_Array_Access :=
        Project.Extended_Projects_Source_Files;
   begin
      for Source_File of Source_Files.all loop
         Graph.Write_Node (Source_File);
      end loop;

      VFS.Unchecked_Free (Source_Files);
   end Extract_Nodes;

   procedure Extract_Edges
     (Project          : GPR.Project_Type; Context : Utilities.Project_Context;
      Recurse_Projects : Boolean; Graph : Graph_Operations.Graph_Context)
   is
      Source_Files : VFS.File_Array_Access :=
        Project.Extended_Projects_Source_Files;
      Main_Files       : VFS.File_Array_Access := null;
      Has_Non_Ada_Main : Boolean               := False;
   begin
      for Source_File of Source_Files.all loop
         Graph.Write_Edge
           (Project, Source_File, Node_Edge_Types.Edge_Type_Contains);

         --  GPRBuild compiles all sources in languages other than Ada.
         if not Utilities.Is_Ada_File_In_Project_Context
             (+Source_File.Full_Name, Context, Recurse_Projects)
           and then Utilities.Is_Buildable_File_In_Project_Context
             (+Source_File.Full_Name, Context)
         then
            Graph.Write_Edge
              (Project, Source_File, Node_Edge_Types.Edge_Type_Compiles);
         end if;

         if Project.Is_Main_File (Source_File.Full_Name) then
            VFS.Append (Main_Files, Source_File);
            Has_Non_Ada_Main :=
              Has_Non_Ada_Main or
              not Utilities.Is_Ada_File_In_Project_Context
                (+Source_File.Full_Name, Context, Recurse_Projects);
         end if;
      end loop;

      VFS.Unchecked_Free (Source_Files);

      --  This is an over-approximation, as we do not handle the
      --  "Roots" attibute; see the description of compilation phase
      --  in the GPRBuild documentation.
      declare
         Built_Files : VFS.File_Array_Access;
      begin
         if Has_Non_Ada_Main
            --  GPRBuild compiles all Ada soruces if there is non-Ada main.
            or else Main_Files = null
            --  GPRBuild compiles all Ada sources if no main is specified.
         then
            Built_Files := Project.Extended_Projects_Source_Files;
         else
            declare
               Status : GPR.Status_Type;
            begin
               --  GPRBuild compiles all Ada files in the closure
               --  of an Ada main.
               Project.Get_Closures
                 (Main_Files, False, False, Status, Built_Files);
            end;
         end if;

         for Built_File of Built_Files.all loop
            if Utilities.Is_Buildable_File_In_Project_Context
                (+Built_File.Full_Name, Context)
            then
               Graph.Write_Edge
                 (Project, Built_File, Node_Edge_Types.Edge_Type_Compiles);
            end if;
         end loop;

         VFS.Unchecked_Free (Built_Files);
      end;

      VFS.Unchecked_Free (Main_Files);
   end Extract_Edges;

end Extraction.Source_Files_From_Projects;
