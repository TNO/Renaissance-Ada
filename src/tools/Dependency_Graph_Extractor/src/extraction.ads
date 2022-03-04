with Ada.Strings.Unbounded;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Libadalang.Analysis;
with Libadalang.Common;
with GraphML_Writers;

package Extraction is

   package GPR   renames GNATCOLL.Projects;
   package GW    renames GraphML_Writers;
   package LAL   renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package SU    renames Ada.Strings.Unbounded;
   package VFS   renames GNATCOLL.VFS;

   function Node_Attributes return GW.Attribute_Definition_Sets.Map;

   function Edge_Attributes return GW.Attribute_Definition_Sets.Map;

   procedure Extract_Dependency_Graph
     (Project_Filename : String;
      Recurse_Projects : Boolean;
      Directory_Prefix : VFS.Virtual_File;
      Graph_File       : in out GW.GraphML_File);

private

   function "="(L, R : LAL.Ada_Node'Class) return Boolean renames LAL."=";

   function "+"(Str : String) return SU.Unbounded_String is
      (SU.To_Unbounded_String (Str));

   Internal_Extraction_Error : exception;

end Extraction;
