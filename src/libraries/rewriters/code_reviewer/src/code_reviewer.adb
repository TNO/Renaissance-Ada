--  TODO: how to deal with virusscanners?
--  Performance can be limited by speed of virus scanner ;-(
--                         running in a single thread...

with Ada.Directories;     use Ada.Directories;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Text_IO;         use Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;
with Rejuvenation;        use Rejuvenation;
with Rejuvenation.Environment_Variables;
use Rejuvenation.Environment_Variables;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Pretty_Print;   use Rejuvenation.Pretty_Print;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Patchers;                    use Patchers;
with Predefined_Patchers;         use Predefined_Patchers;
with Commands;                    use Commands;
with Version_Controls;            use Version_Controls;
with Git_Version_Controls;        use Git_Version_Controls;
with Mark_Utils;                  use Mark_Utils;
with String_Vectors;              use String_Vectors;
with String_Maps;                 use String_Maps;

procedure Code_Reviewer is
   -----------------------------------------------------------
   --  Configuration
   -----------------------------------------------------------

   Source_Directory : constant String := "C:\path\to\semantic_versioning";
   --  "C:\path\to\Dependency_Graph_Extractor-Ada";
   --  Example to review the code within Dependency_Graph_Extractor-Ada

   V_C : constant Version_Control'Class :=
     Make_Git_Version_Control (Source_Directory);

   Project_Filename : constant String :=
     Source_Directory & "\semantic_versioning.gpr";
   --  "\dependency_graph_extractor.gpr";
   --  Example to review the Dependency_Graph_Extractor project

   function Get_Environment_Variables return String_Maps.Map;
   --  Environment Variables for analyzed project
   --
   --  Note: PATH environment variable must include
   --        gnatpp and svn / git for the analysis of code_reviewer
   --
   --  TODO: extract environment variables of alire projects automatically
   --        using `alr printenv` and regular expression matching

   function Get_Environment_Variables return String_Maps.Map is
      Return_Value : String_Maps.Map := String_Maps.Empty;
   begin
      Return_Value.Include ("ALIRE", "True");
      Return_Value.Include
        ("C_INCLUDE_PATH",
         "C:\Users\laarpjljvd\.cache\alire\msys64\mingw64\include");
      Return_Value.Include
        ("GPR_PROJECT_PATH",
         "C:\path\to\semantic_versioning");
      Return_Value.Include
        ("LIBRARY_PATH",
         "C:\Users\laarpjljvd\.cache\alire\msys64\mingw64\lib");
      Return_Value.Include
        ("PATH",
         "C:\Users\laarpjljvd\.cache\alire\msys64\usr\bin;" &
         "C:\Users\laarpjljvd\.cache\alire\msys64\usr\local\bin;" &
         "C:\Users\laarpjljvd\.cache\alire\msys64\mingw64\bin;" &
         "C:\Program Files\TortoiseSVN\bin;" &
         "C:\Program Files\TortoiseGit\bin;" & "C:\Program Files\Git\cmd;" &
         "C:\Program Files\Alire\bin;" & "C:\GNATPRO\23.0w-20220211\bin");
      return Return_Value;
   end Get_Environment_Variables;

   Patchers : constant Patchers_Vectors.Vector := Patchers_Predefined;

   -----------------------------------------------------------
   --  Implementation
   -----------------------------------------------------------

   Error_Count : Natural := 0;

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
                  Pretty_Print_Sections (Filename, Project_Filename);
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

begin
   Set (Get_Environment_Variables);
   declare
      Filenames : constant String_Vectors.Vector := Get_Filenames;
   begin
      V_C.Rewind_Not_Committed_Changes;
      Create_Patches (Filenames, Patchers);
      Put_Line ("### done ###");
   end;
exception
   when Error : others =>
      Put_Line (Exception_Message (Error));
end Code_Reviewer;
