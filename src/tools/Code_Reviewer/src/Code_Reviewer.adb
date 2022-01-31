--  TODO: how to deal with virusscanners?
--  Performance can be limited by speed of virus scanner ;-(
--                         running in a single thread...

with Ada.Containers;    use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;   use Ada.Directories;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--  with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces.C;                use Interfaces.C;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Pretty_Print;   use Rejuvenation.Pretty_Print;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
--  use Rejuvenation.Simple_Factory.Unbounded_Strings;
with Rejuvenation.Text_Rewrites; use Rejuvenation.Text_Rewrites;

with Rewriters;                  use Rewriters;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Predefined_Rewriters;       use Predefined_Rewriters;

procedure Code_Reviewer is

   Error_Count : Natural := 0;

   package Name_To_Rewriter_Maps is new Indefinite_Ordered_Maps
     (Key_Type => String, Element_Type => Rewriter_Find_And_Replace);

   type Version_Control_Kind is (GIT, SVN);
   Source_Version_Control : constant Version_Control_Kind := GIT;

   Source_Directory : constant String := "C:\path\to\Renaissance-Ada";
   --  Example to review the code within Renaissance-Ada

   Project_Filename : constant String :=
     Source_Directory & "\src\libraries\Rejuvenation_Lib\rejuvenation_lib.gpr";
   --  Example to review the rejuvenation_lib project
   --  TODO: when aggregate projects are supported review all projects!

   Invocation_Exception : exception;

   procedure Execute_Command (Command : String);
   procedure Execute_Command (Command : String) is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");

      Ret_Val : Integer;
   begin
      Ret_Val := Sys (To_C (Command));
      if Ret_Val /= 0 then
         raise Invocation_Exception
           with Ret_Val'Image & " for '" & Command & "'";
      end if;
   end Execute_Command;

   procedure Revert_SVN;
   procedure Revert_SVN is
   begin
      Execute_Command ("svn revert --recursive " & Source_Directory);
   end Revert_SVN;

   procedure Restore_GIT;
   procedure Restore_GIT is
   begin
      Execute_Command ("cd """ & Source_Directory & """ & git restore *");
   end Restore_GIT;

   procedure Rewind_Not_Committed_Changes;
   procedure Rewind_Not_Committed_Changes is
   begin
      case Source_Version_Control is
         when SVN =>
            Revert_SVN;
         when GIT =>
            Restore_GIT;
      end case;
   end Rewind_Not_Committed_Changes;

   procedure Diff_SVN (File_Name : String);
   procedure Diff_SVN (File_Name : String) is
   begin
      Execute_Command ("svn diff " & Source_Directory & " > " & File_Name);
   end Diff_SVN;

   procedure Diff_GIT (File_Name : String);
   procedure Diff_GIT (File_Name : String) is
   begin
      Execute_Command ("cd " & Source_Directory & "& git diff > " & File_Name);
   end Diff_GIT;

   procedure Create_Patch (patch : String);
   procedure Create_Patch (patch : String) is
      File_Name : constant String :=
        Compose ("C:\path\to\patches",
      --  Note: path must exist
      --  Path is NOT created by this program!
      patch, "patch");
   begin
      case Source_Version_Control is
         when SVN =>
            Diff_SVN (File_Name);
         when GIT =>
            Diff_GIT (File_Name);
      end case;
   end Create_Patch;

   procedure Change_Files (Units : Analysis_Units.Vector; R : Rewriter'Class);
   procedure Change_Files (Units : Analysis_Units.Vector; R : Rewriter'Class)
   is
   begin
      for Unit of Units loop
         declare
            File             : constant String := Unit.Get_Filename;
            Original_Content : constant String :=
              Encode (Unit.Text, Unit.Get_Charset);
         begin
            declare
               TR      : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
               Content : constant String   := R.Rewrite (Unit.Root);
            begin
               TR.Replace
                 (Unit.Root, Content, Trivia_On_Same_Line, Trivia_On_Same_Line,
                  Unit.Get_Charset);
               if TR.ApplyToString /= Original_Content then
                  Put_Line ("Changed " & File);
                  Turn_Pretty_Printing_Initially_Off (TR);
                  TR.Apply;
                  Pretty_Print_Sections (File, Standard_Options_Project);
                  Remove_Pretty_Print_Flags (File);
               end if;
            exception
               when Error : others =>
                  declare
                     Error_File_Name : constant String :=
                       "c:\Temp\error" & Trim (Error_Count'Image, Both) &
                       ".adx";
                  begin
                     Put_Line
                       ("Error in Change_Files - " & File & " " &
                        Exception_Message (Error));
                     Execute_Command ("move " & File & " " & Error_File_Name);
                     Put_Line ("See " & Error_File_Name);
                     Write_String_To_File (Original_Content, File);
                     Error_Count := Error_Count + 1;
                  end;
            end;
         exception
            when Error : Parse_Exception =>
               Put_Line
                 ("Not an Ada file - " & File & " " &
                  Exception_Message (Error));
         end;
      end loop;
   end Change_Files;

   procedure Create_Patches
     (Units : Analysis_Units.Vector; Map : Name_To_Rewriter_Maps.Map);
   procedure Create_Patches
     (Units : Analysis_Units.Vector; Map : Name_To_Rewriter_Maps.Map)
   is
   begin
      for KE in Map.Iterate loop
         declare
            Rewriter_Name : constant String := Name_To_Rewriter_Maps.Key (KE);
         begin
            Put_Line ("==== " & Rewriter_Name & " ====");
            Change_Files (Units, Name_To_Rewriter_Maps.Element (KE));
            Create_Patch (Rewriter_Name);
            Rewind_Not_Committed_Changes;
         end;
      end loop;
   end Create_Patches;

   procedure Report_Count_Instances
     (Units : Analysis_Units.Vector; Map : Name_To_Rewriter_Maps.Map);
   procedure Report_Count_Instances
     (Units : Analysis_Units.Vector; Map : Name_To_Rewriter_Maps.Map)
   is
   begin
      for KE in Map.Iterate loop
         declare
            Rewriter_Name : constant String  := Name_To_Rewriter_Maps.Key (KE);
            F_P           : constant Pattern :=
              Name_To_Rewriter_Maps.Element (KE).Find_Pattern;
            Count : Natural := 0;
         begin
            for Unit of Units loop
               Count :=
                 Count +
                 Natural
                   (if F_P.As_Ada_Node.Kind in Ada_Ada_List then
                      Find_Sub_List (Unit.Root, F_P).Length
                    else Find_Full (Unit.Root, F_P).Length);
            end loop;
            Put_Line (Rewriter_Name & " : " & Count'Image);
         end;
      end loop;

   end Report_Count_Instances;

   function Get_Units return Analysis_Units.Vector;
   function Get_Units return Analysis_Units.Vector is
   begin
      return Analyze_Project (Project_Filename);
   end Get_Units;

   --  function Get_Units return Analysis_Units.Vector is
   --     UFiles : constant Unbounded_Strings.Vector :=
   --       Get_Ada_Source_Files_From_Directory (Source_Directory);
   --     Units : Analysis_Units.Vector;
   --  begin
   --     for UFile of UFiles loop
   --        begin
   --           declare
   --              File : constant String        := To_String (UFile);
   --              Unit : constant Analysis_Unit := Analyze_File (File);
   --           begin
   --              Units.Append (Unit);
   --           end;
   --        exception
   --           when Parse_Exception =>
   --              null;
   --        end;
   --     end loop;
   --     return Units;
   --  end Get_Units;

   function Get_Map return Name_To_Rewriter_Maps.Map;
   function Get_Map return Name_To_Rewriter_Maps.Map is
      Name_To_Rewriter_Map : Name_To_Rewriter_Maps.Map;
   begin
      --  Name_To_Rewriter_Map.Include
      --    ("Minimal_Parentheses", RMP);

      Name_To_Rewriter_Map.Include
        ("Definition_Equal", Rewriter_Definition_Equal);
      Name_To_Rewriter_Map.Include
        ("Definition_Different", Rewriter_Definition_Different);
      Name_To_Rewriter_Map.Include
        ("Definition_Minus", Rewriter_Definition_Minus);
      Name_To_Rewriter_Map.Include
        ("Definition_Divide", Rewriter_Definition_Divide);
      Name_To_Rewriter_Map.Include
        ("Definition_Modulo", Rewriter_Definition_Modulo);
      Name_To_Rewriter_Map.Include
        ("Definition_Remainder", Rewriter_Definition_Remainder);

      Name_To_Rewriter_Map.Include
        ("Idempotence_And", Rewriter_Idempotence_And);
      Name_To_Rewriter_Map.Include ("Idempotence_Or", Rewriter_Idempotence_Or);
      Name_To_Rewriter_Map.Include
        ("Complementation_And", Rewriter_Complementation_And);
      Name_To_Rewriter_Map.Include
        ("Complementation_Or", Rewriter_Complementation_Or);

      Name_To_Rewriter_Map.Include ("Not_Not", Rewriter_Not_Not);
      Name_To_Rewriter_Map.Include ("Not_Equal", Rewriter_Not_Equal);
      Name_To_Rewriter_Map.Include ("Not_Different", Rewriter_Not_Different);
      Name_To_Rewriter_Map.Include
        ("Not_Greater_Than", Rewriter_Not_Greater_Than);
      Name_To_Rewriter_Map.Include
        ("Not_Greater_Equal", Rewriter_Not_Greater_Equal);
      Name_To_Rewriter_Map.Include ("Not_Less_Than", Rewriter_Not_Less_Than);
      Name_To_Rewriter_Map.Include ("Not_Less_Equal", Rewriter_Not_Less_Equal);
      Name_To_Rewriter_Map.Include ("Not_In", Rewriter_Not_In);
      Name_To_Rewriter_Map.Include ("Not_Not_In", Rewriter_Not_Not_In);
      Name_To_Rewriter_Map.Include ("And_Then", Rewriter_And_Then);
      Name_To_Rewriter_Map.Include ("Or_Else", Rewriter_Or_Else);
      Name_To_Rewriter_Map.Include ("Equal_True", Rewriter_Equal_True);
      Name_To_Rewriter_Map.Include ("Equal_False", Rewriter_Equal_False);
      Name_To_Rewriter_Map.Include ("Different_True", Rewriter_Different_True);
      Name_To_Rewriter_Map.Include
        ("Different_False", Rewriter_Different_False);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_And", Rewrite_De_Morgan_Not_And);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_Or", Rewrite_De_Morgan_Not_Or);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_All_Range", Rewrite_De_Morgan_Not_All_Range);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_All_Elements", Rewrite_De_Morgan_Not_All_Elements);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_Some_Range", Rewrite_De_Morgan_Not_Some_Range);
      Name_To_Rewriter_Map.Include
        ("De_Morgan_Not_Some_Elements", Rewrite_De_Morgan_Not_Some_Elements);
      Name_To_Rewriter_Map.Include
        ("If_Different_Expression", Rewriter_If_Different_Expression);
      Name_To_Rewriter_Map.Include
        ("If_Not_Condition_Expression",
         Rewriter_If_Not_Condition_Expression);
      Name_To_Rewriter_Map.Include
        ("If_Not_In_Expression", Rewriter_If_Not_In_Expression);
      Name_To_Rewriter_Map.Include
        ("Boolean_If_Condition_Expression",
         Rewriter_Boolean_If_Condition_Expression);
      Name_To_Rewriter_Map.Include
        ("Boolean_If_Not_Condition_Expression",
         Rewriter_Boolean_If_Not_Condition_Expression);
      Name_To_Rewriter_Map.Include
        ("Concat_Before_If_Expression",
         Rewriter_Concat_Before_If_Expression);
      Name_To_Rewriter_Map.Include
        ("Concat_After_If_Expression", Rewriter_Concat_After_If_Expression);
      Name_To_Rewriter_Map.Include
        ("Case_Expression_Binary_With_Others",
         Rewriter_Case_Expression_Binary_With_Others);
      Name_To_Rewriter_Map.Include
        ("Equals_To_In_Range", Rewriter_Equals_To_In_Range);
      Name_To_Rewriter_Map.Include
        ("Combine_In_Range_And_Equal", Rewriter_Combine_In_Range_And_Equal);
      Name_To_Rewriter_Map.Include
        ("Combine_In_Ranges", Rewriter_Combine_In_Ranges);
      Name_To_Rewriter_Map.Include
        ("Differents_To_Not_In_Range", Rewriter_Differents_To_Not_In_Range);
      Name_To_Rewriter_Map.Include
        ("Combine_Not_In_Range_And_Different",
         Rewriter_Combine_Not_In_Range_And_Different);
      Name_To_Rewriter_Map.Include
        ("Combine_In_Ranges", Rewriter_Combine_In_Ranges);
      Name_To_Rewriter_Map.Include ("Double", Rewriter_Double);

      Name_To_Rewriter_Map.Include
        ("Unnecessary_Null_Stmt", Rewriter_Unnecessary_Null_Stmt);
      Name_To_Rewriter_Map.Include ("If_True_Stmt", Rewriter_If_True_Stmt);
      Name_To_Rewriter_Map.Include ("If_False_Stmt", Rewriter_If_False_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Different_Stmt", Rewriter_If_Different_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Not_Condition_Stmt", Rewriter_If_Not_Condition_Stmt);
      Name_To_Rewriter_Map.Include ("If_Not_In_Stmt", Rewriter_If_Not_In_Stmt);
      Name_To_Rewriter_Map.Include ("Use_Elsif", Rewriter_Use_Elsif);
      Name_To_Rewriter_Map.Include
        ("Null_Then_Branch", Rewriter_Null_Then_Branch);
      Name_To_Rewriter_Map.Include
        ("Null_Else_Branch", Rewriter_Null_Else_Branch);
      Name_To_Rewriter_Map.Include
        ("If_Identical_Branches_Stmt", Rewriter_If_Identical_Branches_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Identical_Tails_Stmt", Rewriter_If_Identical_Tails_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Argument_Stmt", Rewriter_If_Argument_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Assignment_Stmt", Rewriter_If_Assignment_Stmt);
      Name_To_Rewriter_Map.Include ("If_Return_Stmt", Rewriter_If_Return_Stmt);
      Name_To_Rewriter_Map.Include
        ("If_Return_Stmts", Rewriter_If_Return_Stmts);

      Name_To_Rewriter_Map.Include ("Case_Single", Rewriter_Case_Single);
      Name_To_Rewriter_Map.Include
        ("Case_Binary_With_Others", Rewriter_Case_Binary_With_Others);
      Name_To_Rewriter_Map.Include
        ("Return_Expression", Rewriter_Return_Expression);

      Name_To_Rewriter_Map.Include
        ("For_All_Range_And_Then", Rewriter_For_All_Range_And_Then);
      Name_To_Rewriter_Map.Include
        ("For_All_Elements_And_Then", Rewriter_For_All_Elements_And_Then);
      Name_To_Rewriter_Map.Include
        ("For_Some_Range_Or_Else", Rewriter_For_Some_Range_Or_Else);
      Name_To_Rewriter_Map.Include
        ("For_Some_Elements_Or_Else", Rewriter_For_Some_Elements_Or_Else);
      Name_To_Rewriter_Map.Include
        ("For_All_Range_Exit", Rewriter_For_All_Range_Exit);
      Name_To_Rewriter_Map.Include
        ("For_All_Elements_Exit", Rewriter_For_All_Elements_Exit);
      Name_To_Rewriter_Map.Include
        ("For_Some_Range_Exit", Rewriter_For_Some_Range_Exit);
      Name_To_Rewriter_Map.Include
        ("For_Some_Elements_Exit", Rewriter_For_Some_Elements_Exit);
      Name_To_Rewriter_Map.Include
        ("For_All_Range_Return", Rewriter_For_All_Range_Return);
      Name_To_Rewriter_Map.Include
        ("For_All_Elements_Return", Rewriter_For_All_Elements_Return);
      Name_To_Rewriter_Map.Include
        ("For_Some_Range_Return", Rewriter_For_Some_Range_Return);
      Name_To_Rewriter_Map.Include
        ("For_Some_Elements_Return", Rewriter_For_Some_Elements_Return);
      Name_To_Rewriter_Map.Include
        ("For_All_Range_All", Rewriter_For_All_Range_All);
      Name_To_Rewriter_Map.Include
        ("For_All_Elements_All", Rewriter_For_All_Elements_All);
      Name_To_Rewriter_Map.Include
        ("For_Some_Range_All", Rewriter_For_Some_Range_All);
      Name_To_Rewriter_Map.Include
        ("For_Some_Elements_All", Rewriter_For_Some_Elements_All);

      Name_To_Rewriter_Map.Include
        ("Declare_And_Overwrite", Rewriter_Declare_And_Overwrite);
      Name_To_Rewriter_Map.Include
        ("Declarations_Combine", Rewriter_Declarations_Combine);
      Name_To_Rewriter_Map.Include
        ("For_Attribute_Use", Rewriter_For_Attribute_Use);
      Name_To_Rewriter_Map.Include
        ("For_Attribute_Use_Aliased", Rewriter_For_Attribute_Use_Aliased);
      Name_To_Rewriter_Map.Include
        ("For_Attribute_Use_Array", Rewriter_For_Attribute_Use_Array);
      Name_To_Rewriter_Map.Include
        ("For_Attribute_Use_Pragma_Var",
         Rewriter_For_Attribute_Use_Pragma_Var);
      Name_To_Rewriter_Map.Include
        ("For_Attribute_Use_Pragma_All",
         Rewriter_For_Attribute_Use_Pragma_All);

      return Name_To_Rewriter_Map;
   end Get_Map;

   Units                : constant Analysis_Units.Vector     := Get_Units;
   Name_To_Rewriter_Map : constant Name_To_Rewriter_Maps.Map := Get_Map;
begin
   Rewind_Not_Committed_Changes;
   Report_Count_Instances (Units, Name_To_Rewriter_Map);
   Create_Patches (Units, Name_To_Rewriter_Map);
end Code_Reviewer;
