with Extraction.Node_Edge_Types;
with Ada.Text_IO;

package body Extraction.File_System is

   use type VFS.File_Array_Access;
   use type VFS.Filesystem_String;

   function All_Relevant_Files (Directory : VFS.Virtual_File) return VFS.File_Array_Access
     with Pre => Directory.Is_Directory
   --  Only analyse relevant files
   --  I.e. remove irrelevant files and directories
   --  (e.g. version management related directories and files)
   is
      Result : VFS.File_Array_Access;

      procedure Internal (Directory : VFS.Virtual_File) is
         Files        : VFS.File_Array_Access := Directory.Read_Dir;
      begin
         for File of Files.all loop
            if File.Is_Directory then
               declare
                  Base_Dir_Name : constant String := +File.Base_Dir_Name;
               begin
                  if Base_Dir_Name (Base_Dir_Name'First) = '.' then
                     Ada.Text_IO.Put_Line ("Skipping " & Base_Dir_Name);
                  else
                     VFS.Append (Result, File);
                     Internal (File);
                  end if;
               end;
            else
               --  No Filter on Files (yet)
               VFS.Append (Result, File);
            end if;
         end loop;

         VFS.Unchecked_Free(Files);
      end Internal;

   begin
      Internal (Directory);
      return Result;
   end All_Relevant_Files;

   procedure Extract_Nodes
     (Directory : VFS.Virtual_File;
      Graph     : Graph_Operations.Graph_Context)
     --  Add all relevant files in the file system.
     --  This enables the finding of "dead files":
     --  Files in the archive but no longer compiled / used by any project.
   is
      Files : VFS.File_Array_Access := All_Relevant_Files (Directory);
   begin
      Graph.Write_Node(Directory);

      if Files /= null then
         for File of Files.all loop
            Graph.Write_Node (File);
         end loop;
      end if;

      VFS.Unchecked_Free(Files);
   end Extract_Nodes;

   procedure Extract_Edges
     (Directory : VFS.Virtual_File;
      Graph     : Graph_Operations.Graph_Context)
   is
      Files : VFS.File_Array_Access := All_Relevant_Files (Directory);
   begin
      if Files /= null then
         for File of Files.all loop
            Graph.Write_Edge(File.Get_Parent, File, Node_Edge_Types.Edge_Type_Contains);
         end loop;
      end if;

      VFS.Unchecked_Free(Files);
   end Extract_Edges;

end Extraction.File_System;
