with Extraction.Node_Edge_Types;
with Ada.Text_IO;

package body Extraction.File_System is

   use type VFS.File_Array_Access;
   use type VFS.Filesystem_String;

   function All_Relevant_Files
     (Directory : VFS.Virtual_File) return VFS.File_Array_Access with
      Pre => Directory.Is_Directory;
      --  Only analyse relevant files
      --  I.e. remove irrelevant files and directories
      --  (e.g. version management related directories and files)
   function All_Relevant_Files
         (Directory : VFS.Virtual_File) return VFS.File_Array_Access
   is
      Result : VFS.File_Array_Access;

      function Is_Hidden (File : VFS.Virtual_File) return Boolean;
      function Is_Hidden (File : VFS.Virtual_File) return Boolean
         --  Linux-style hidden files and directories start with a '.'
         is
         Base_Name : constant String := +File.Base_Name;
      begin
         return Base_Name (Base_Name'First) = '.';
      end Is_Hidden;

      procedure Internal (Directory : VFS.Virtual_File);
      procedure Internal (Directory : VFS.Virtual_File) is
         Files : VFS.File_Array_Access := Directory.Read_Dir;
      begin
         for File of Files.all loop
            if Is_Hidden (File) then
               Ada.Text_IO.Put_Line ("Skipping " & (+File.Full_Name));
            else
               VFS.Append (Result, File);
               if File.Is_Directory then
                  Internal (File);
               end if;
            end if;
         end loop;

         VFS.Unchecked_Free (Files);
      end Internal;

   begin
      Internal (Directory);
      return Result;
   end All_Relevant_Files;

   procedure Extract_Nodes
     (Directory : VFS.Virtual_File; Graph : Graph_Operations.Graph_Context)
      --  Add all relevant files in the file system.
      --  This enables the finding of "dead files":
      --  Files in the archive but no longer compiled / used by any project.

   is
      Files : VFS.File_Array_Access := All_Relevant_Files (Directory);
   begin
      Graph.Write_Node (Directory);

      if Files /= null then
         for File of Files.all loop
            Graph.Write_Node (File);
         end loop;
      end if;

      VFS.Unchecked_Free (Files);
   end Extract_Nodes;

   procedure Extract_Edges
     (Directory : VFS.Virtual_File; Graph : Graph_Operations.Graph_Context)
   is
      Files : VFS.File_Array_Access := All_Relevant_Files (Directory);
   begin
      if Files /= null then
         for File of Files.all loop
            Graph.Write_Edge
              (File.Get_Parent, File, Node_Edge_Types.Edge_Type_Contains);
         end loop;
      end if;

      VFS.Unchecked_Free (Files);
   end Extract_Edges;

end Extraction.File_System;
