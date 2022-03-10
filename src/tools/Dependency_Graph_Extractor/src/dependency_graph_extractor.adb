with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.VFS;
with Command_Line;
with Extraction;
with GraphML_Writers;

procedure Dependency_Graph_Extractor is
   package G_W renames GraphML_Writers;
   package S_U renames Ada.Strings.Unbounded;
   package V_F_S renames GNATCOLL.VFS;

   use type Ada.Calendar.Time;
   use type S_U.Unbounded_String;
   use type V_F_S.Filesystem_String;
   use type V_F_S.Virtual_File;

   Output_File      : S_U.Unbounded_String;
   Directory_Prefix : S_U.Unbounded_String;
   Recurse_Projects : Boolean;
   Input_Files      : Command_Line.Input_File_Vectors.Vector;
   Start_Time       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
begin
   if not Command_Line.Parse_Command_Line
       (Input_Files, Recurse_Projects, Directory_Prefix, Output_File)
   then
      return;
   end if;

   if Output_File = S_U.Null_Unbounded_String then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "No output file provided");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   declare
      use Extraction;

      Filename : constant String           := S_U.To_String (Output_File);
      Prefix   : constant V_F_S.Virtual_File :=
        (if Directory_Prefix = S_U.Null_Unbounded_String then V_F_S.No_File
         else V_F_S.Create_From_Base (+S_U.To_String (Directory_Prefix)));
      Graph : G_W.GraphML_File :=
        G_W.Create_GraphML_Writer (Filename, Node_Attributes, Edge_Attributes);
   begin
      Prefix.Normalize_Path;
      for Input_File of Input_Files loop
         Extract_Dependency_Graph
           (S_U.To_String (Input_File), Recurse_Projects, Prefix, Graph);
      end loop;
      Graph.Close;
   end;

   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      Duration'Image (Ada.Calendar.Clock - Start_Time));
end Dependency_Graph_Extractor;
