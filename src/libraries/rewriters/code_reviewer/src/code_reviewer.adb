--  TODO: how to deal with virusscanners?
--  Performance can be limited by speed of virus scanner ;-(
--                         running in a single thread...

with Ada.Directories;             use Ada.Directories;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Text_IO;                 use Ada.Text_IO;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Pretty_Print;   use Rejuvenation.Pretty_Print;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Patchers;                    use Patchers;
with Predefined_Patchers;         use Predefined_Patchers;

with Commands;             use Commands;
with Version_Controls;     use Version_Controls;
with SVN_Version_Controls; use SVN_Version_Controls;
--  with Git_Version_Controls; use Git_Version_Controls;
with Mark_Utils;     use Mark_Utils;
with String_Vectors; use String_Vectors;

procedure Code_Reviewer is

   Error_Count : Natural := 0;

   Source_Directory : constant String :=
   --  "C:\path\to\Dependency_Graph_Extractor-Ada";
   "C:\bright\itecembed";
   --  Example to review the code within Dependency_Graph_Extractor-Ada

   V_C : constant Version_Control'Class :=
     Make_SVN_Version_Control (Source_Directory);
   --  Make_Git_Version_Control (Source_Directory);

   Project_Filename : constant String := Source_Directory & "\Source\itec.gpr";
   --  "\dependency_graph_extractor.gpr";
   --  Example to review the Dependency_Graph_Extractor project

   procedure Change_Files
     (Filenames : String_Vectors.Vector; P : Patcher'Class);
   procedure Change_Files
     (Filenames : String_Vectors.Vector; P : Patcher'Class)
   --  Note Pretty Printing (using gnatpp) is line based.
   --  Rewrite contexts can be more precise,
   --  so we have separate marks for (post processing by) rewriters
   --  and pretty printing

   is

   begin
      for Filename of Filenames loop
         declare
            Original_Content : constant String :=
              Get_String_From_File (Filename);
         begin
            declare
               Unit : Analysis_Unit :=
                 Analyze_File_In_Project (Filename, Project_Filename);
            begin
               if P.Prepare_Unit (Unit) then
                  Put_Line ("--- " & Filename & " ---");
                  P.Rewrite (Unit);
                  Remove_Marks (Filename);
                  Pretty_Print_Sections (Filename);
                  --  , Project_Filename);
                  --  TODO: have gnatpp use the correct project environment
                  --        currently, we get the environment of the
                  --        analysing i.s.o. analysed project
                  Remove_Pretty_Print_Flags (Filename);
               end if;
            end;
         exception
            when Error : others =>
               declare
                  Error_File_Name : constant String :=
                    "c:\Temp\error" & Trim (Error_Count'Image, Both) & ".adx";
               begin
                  Put_Line
                    ("Error in Change_Files - " & Filename & " " &
                     Exception_Message (Error));
                  Execute_Command ("move " & Filename & " " & Error_File_Name);
                  Put_Line ("See " & Error_File_Name);
                  Write_String_To_File (Original_Content, Filename);
                  Error_Count := Error_Count + 1;
               end;
         end;
      end loop;
   end Change_Files;

   procedure Create_Patches
     (Filenames : String_Vectors.Vector; Ps : Patchers_Vectors.Vector);
   procedure Create_Patches
     (Filenames : String_Vectors.Vector; Ps : Patchers_Vectors.Vector)
   is
   begin
      for P of Ps loop
         declare
            Patch_Filename : constant String :=
              Compose ("C:\path\to\patches",
                       --  Note: path must exist
                       --  Path is NOT created by this program!
                       P.Name, "patch");
         begin
            Put_Line ("==== " & P.Name & " ====");
            Change_Files (Filenames, P);
            V_C.Create_Patch (Patch_Filename);
            V_C.Rewind_Not_Committed_Changes;
         end;
      end loop;
   end Create_Patches;

   function Get_Filenames return String_Vectors.Vector;
   function Get_Filenames return String_Vectors.Vector is
      Project_Filenames : constant String_Vectors.Vector :=
        Get_Ada_Source_Files_From_Project (Project_Filename);
      Return_Value : String_Vectors.Vector;
   begin
      for Project_Filename of Project_Filenames loop
         if V_C.Is_Under_Version_Control (Project_Filename) then
            Return_Value.Append (Project_Filename);
         end if;
      end loop;
      return Return_Value;
   end Get_Filenames;

   Filenames : constant String_Vectors.Vector   := Get_Filenames;
   Patchers  : constant Patchers_Vectors.Vector := Patchers_Predefined;
   --  Patchers_Vectors.To_Vector (Patcher_Declarations_Combine, 1);
begin
   V_C.Rewind_Not_Committed_Changes;
   Create_Patches (Filenames, Patchers);
exception
   when Error : others =>
      Put_Line (Exception_Message (Error));
end Code_Reviewer;
