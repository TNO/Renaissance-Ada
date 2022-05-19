--  TODO: how to deal with virusscanners?
--  Performance can be limited by speed of virus scanner ;-(
--                         running in a single thread...

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis; use Libadalang.Analysis;
with Rejuvenation; use Rejuvenation;
with Rejuvenation.File_Utils; use Rejuvenation.File_Utils;
with Rejuvenation.Finder; use Rejuvenation.Finder;
with Rejuvenation.Pretty_Print; use Rejuvenation.Pretty_Print;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Text_Rewrites; use Rejuvenation.Text_Rewrites;
with Patchers; use Patchers;
with Predefined_Patchers; use Predefined_Patchers;

with Commands; use Commands;
with Version_Controls; use Version_Controls;
with SVN_Version_Controls; use SVN_Version_Controls;
--  with Git_Version_Controls; use Git_Version_Controls;
with Mark_Utils; use Mark_Utils;

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

   procedure Add_Pretty_Print (Unit : Analysis_Unit);
   procedure Add_Pretty_Print (Unit : Analysis_Unit)
   is
      T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      Nodes : constant Node_List.Vector :=
        Find_Non_Contained (Unit.Root, Is_Marked'Access);
   begin
      for Node of Nodes loop
         Surround_Node_By_Pretty_Print_Section (T_R, Node);
      end loop;
      Turn_Pretty_Printing_Initially_Off (T_R);
      T_R.Apply;
   end Add_Pretty_Print;

   procedure Change_Files (Units : Analysis_Units.Vector; P : Patcher'Class);
   procedure Change_Files (Units : Analysis_Units.Vector; P : Patcher'Class)
     --  Note Pretty Printing (using gnatpp) is line based.
     --  Rewrite contexts can be more precise,
     --  so we have separate marks for (post processing by) rewriters
     --  and pretty printing
   is
   begin
      for Unit of Units loop
         begin
            declare
               Current_Unit : Analysis_Unit := Unit;
               --  prevent error: actual for "Unit" must be a variable
            begin
               if P.Mark (Current_Unit) then
                  Put_Line ("--- " & Unit.Get_Filename & " ---");
                  P.Rewrite (Current_Unit);
                  Add_Pretty_Print (Current_Unit);
                  Remove_Marks (Current_Unit.Get_Filename);
                  Pretty_Print_Sections
                    (Current_Unit.Get_Filename);
                  --  , Project_Filename);
                  --  TODO: have gnatpp use the correct project environment
                  --        currently, we get the environment of the
                  --        analysing i.s.o. analysed project
                  Remove_Pretty_Print_Flags (Current_Unit.Get_Filename);
               end if;
            end;
         exception
               when Error : others =>
                  declare
                     Error_File_Name : constant String :=
                       "c:\Temp\error" & Trim (Error_Count'Image, Both)
                       & ".adx";
                  begin
                     Put_Line
                       ("Error in Change_Files - " & Unit.Get_Filename & " " &
                          Exception_Message (Error));
                     Execute_Command
                       ("move " & Unit.Get_Filename & " " & Error_File_Name);
                     Put_Line ("See " & Error_File_Name);
                     Write_String_To_File
                       (Encode (Unit.Text, Unit.Get_Charset),
                        Unit.Get_Filename);
                     Error_Count := Error_Count + 1;
                  end;
         end;
      end loop;
   end Change_Files;

   procedure Create_Patches
     (Units : Analysis_Units.Vector; Ps : Patchers_Vectors.Vector);
   procedure Create_Patches
     (Units : Analysis_Units.Vector; Ps : Patchers_Vectors.Vector)
   is
   begin
      for P of Ps loop
         declare
            File_Name : constant String :=
              Compose ("C:\path\to\patches",
                       --  Note: path must exist
                       --  Path is NOT created by this program!
                       P.Name, "patch");
         begin
            Put_Line ("==== " & P.Name & " ====");
            Change_Files (Units, P);
            V_C.Create_Patch (File_Name);
            V_C.Rewind_Not_Committed_Changes;
         end;
      end loop;
   end Create_Patches;

   function Get_Units return Analysis_Units.Vector;
   function Get_Units return Analysis_Units.Vector
   is
      Project_Units : constant Analysis_Units.Vector :=
        Analyze_Project (Project_Filename);
      Return_Value : Analysis_Units.Vector;
   begin
      for Project_Unit of Project_Units loop
         if V_C.Is_Under_Version_Control (Project_Unit.Get_Filename) then
            Return_Value.Append (Project_Unit);
         end if;
      end loop;
      return Return_Value;
   end Get_Units;

   Units : constant Analysis_Units.Vector := Get_Units;
   Patchers : constant Patchers_Vectors.Vector := Patchers_Predefined;
begin
   V_C.Rewind_Not_Committed_Changes;
   Create_Patches (Units, Patchers);
exception
   when Error : others =>
      Put_Line (Exception_Message (Error));
end Code_Reviewer;
