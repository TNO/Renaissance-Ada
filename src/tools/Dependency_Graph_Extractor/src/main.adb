with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.VFS;
with Command_Line;
with Extraction;
with GraphML_Writers;

procedure Main is
   package GW  renames GraphML_Writers;
   package SU  renames Ada.Strings.Unbounded;
   package VFS renames GNATCOLL.VFS;

   use type Ada.Calendar.Time;
   use type SU.Unbounded_String;
   use type VFS.Filesystem_String;
   use type VFS.Virtual_File;

   Output_File      : SU.Unbounded_String;
   Directory_Prefix : SU.Unbounded_String;
   Recurse_Projects : Boolean;
   Input_Files      : Command_Line.Input_File_Vectors.Vector;
   Start_Time       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
begin
   if not Command_Line.Parse_Command_Line(Input_Files, Recurse_Projects, Directory_Prefix, Output_File) then
      return;
   end if;

   if Output_File = SU.Null_Unbounded_String then
      Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "No output file provided");
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;

   declare
      use Extraction;

      Filename : constant String           := SU.To_String(Output_File);
      Prefix   : constant VFS.Virtual_File := (if Directory_Prefix = SU.Null_Unbounded_String
                                               then VFS.No_File
                                               else VFS.Create_From_Base(+SU.To_String(Directory_Prefix)));
      Graph    : GW.GraphML_File           := GW.Create_GraphML_Writer(Filename, Node_Attributes, Edge_Attributes);
   begin
      Prefix.Normalize_Path;
      for Input_File of Input_Files loop
         Extract_Dependency_Graph(SU.To_String(Input_File), Recurse_Projects, Prefix, Graph);
      end loop;
      Graph.Close;
   end;

   Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, Duration'Image(Ada.Calendar.Clock - Start_Time));
end Main;

