--  TODO: how to deal with virusscanners?
--  Performance can be limited by speed of virus scanner ;-(
--                         running in a single thread...

with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;             use Ada.Directories;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces.C;                use Interfaces.C;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rewriters_Sequence;          use Rewriters_Sequence;
with Rewriters_Vectors;           use Rewriters_Vectors;
with Predefined_Rewriters_Block_Statement_Simplify;
use Predefined_Rewriters_Block_Statement_Simplify;
with Predefined_Rewriters_Declaration_Simplify;
use Predefined_Rewriters_Declaration_Simplify;
with Predefined_Rewriters_If_Expression_Distribution;
use Predefined_Rewriters_If_Expression_Distribution;
with Predefined_Rewriters_If_Expression_Simplify;
use Predefined_Rewriters_If_Expression_Simplify;
with Predefined_Rewriters_Not;  use Predefined_Rewriters_Not;
with Patchers;                  use Patchers;
with Patchers_Find_And_Replace; use Patchers_Find_And_Replace;

procedure Code_Reviewer is

   Error_Count : Natural := 0;

   package Patchers_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Patcher'Class);

   type Version_Control_Kind is (GIT, SVN);
   Source_Version_Control : constant Version_Control_Kind := SVN;

   Source_Directory : constant String :=
   --  "C:\path\to\Dependency_Graph_Extractor-Ada";
   "C:\bright\itecembed";
   --  Example to review the code within Dependency_Graph_Extractor-Ada

   Project_Filename : constant String := Source_Directory & "\Source\itec.gpr";
   --  "\dependency_graph_extractor.gpr";
   --  Example to review the Dependency_Graph_Extractor project

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

   procedure Change_Files (Units : Analysis_Units.Vector; P : Patcher'Class);
   procedure Change_Files (Units : Analysis_Units.Vector; P : Patcher'Class) is
   begin
      for Unit of Units loop
         declare
            Current_Unit : Analysis_Unit := Unit;
            --  prevent error: actual for "Unit" must be a variable
         begin
            P.Mark (Current_Unit);
            P.Rewrite (Current_Unit);
            --  TODO remove marks & pretty print
         exception
            when Error : others =>
               declare
                  Error_File_Name : constant String :=
                    "c:\Temp\error" & Trim (Error_Count'Image, Both) & ".adx";
               begin
                  Put_Line
                    ("Error in Change_Files - " & Unit.Get_Filename & " " &
                     Exception_Message (Error));
                  Execute_Command
                    ("move " & Unit.Get_Filename & " " & Error_File_Name);
                  Put_Line ("See " & Error_File_Name);
                  Write_String_To_File
                    (Encode (Unit.Text, Unit.Get_Charset), Unit.Get_Filename);
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
         begin
            Put_Line ("==== " & P.Name & " ====");
            Change_Files (Units, P);
            Create_Patch (P.Name);
            Rewind_Not_Committed_Changes;
         end;
      end loop;
   end Create_Patches;

   function Get_Units return Analysis_Units.Vector;
   function Get_Units return Analysis_Units.Vector is
   begin
      --  --  Handle alr project
   --  Execute_Command ("alr printenv");
   --  --  ensure printenv doesn't generate unexpected output
   --  --  for details: see https://github.com/alire-project/alire/issues/989
   --  Execute_Command
   --  ("for /F ""usebackq delims="" %x in (`alr printenv --wincmd`) DO %x");
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

   function Get_Patchers return Patchers_Vectors.Vector;
   function Get_Patchers return Patchers_Vectors.Vector is
      Vector : Patchers_Vectors.Vector := Patchers_Vectors.Empty;
   begin
      --  TODO: make Predefined Patchers?
      Vector.Append
        (Make_Patcher_Find_And_Replace
           ("Declare_And_Overwrite", Rewriter_Declare_And_Overwrite,
            Make_Rewriter_Sequence
              (Rewriter_If_Expression_Distribution &
               Rewriter_If_Expression_Simplify &
               Rewriter_Not    -- TODO add Minimal Parentheses
      )));

      Vector.Append
        (Make_Patcher_Find_And_Replace
           ("Declarations_Combine", Rewriter_Declarations_Combine));

      return Vector;
   end Get_Patchers;

   --  Name_To_Rewriter_Map.Include
   --    ("Minimal_Parentheses", RMP);

   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Equal", Rewriter_Definition_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Different", Rewriter_Definition_Different);
   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Minus", Rewriter_Definition_Minus);
   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Divide", Rewriter_Definition_Divide);
   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Modulo", Rewriter_Definition_Modulo);
   --  Name_To_Rewriter_Map.Include
   --    ("Definition_Remainder", Rewriter_Definition_Remainder);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("Idempotence_And", Rewriter_Idempotence_And);
   --  Name_To_Rewriter_Map.Include
   --    ("Idempotence_Or", Rewriter_Idempotence_Or);
   --  Name_To_Rewriter_Map.Include
   --    ("Complementation_And", Rewriter_Complementation_And);
   --  Name_To_Rewriter_Map.Include
   --    ("Complementation_Or", Rewriter_Complementation_Or);
   --
   --  Name_To_Rewriter_Map.Include ("Not_Not", Rewriter_Not_Not);
   --  Name_To_Rewriter_Map.Include ("Not_Equal", Rewriter_Not_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Not_Different", Rewriter_Not_Different);
   --  Name_To_Rewriter_Map.Include
   --    ("Not_Greater_Than", Rewriter_Not_Greater_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Not_Greater_Equal", Rewriter_Not_Greater_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Not_Less_Than", Rewriter_Not_Less_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Not_Less_Equal", Rewriter_Not_Less_Equal);
   --  Name_To_Rewriter_Map.Include ("Not_In", Rewriter_Not_In);
   --  Name_To_Rewriter_Map.Include ("Not_Not_In", Rewriter_Not_Not_In);
   --  Name_To_Rewriter_Map.Include ("And_Then", Rewriter_And_Then);
   --  Name_To_Rewriter_Map.Include ("Or_Else", Rewriter_Or_Else);
   --  Name_To_Rewriter_Map.Include ("Equal_True", Rewriter_Equal_True);
   --  Name_To_Rewriter_Map.Include
   --    ("Equal_False", Rewriter_Equal_False);
   --  Name_To_Rewriter_Map.Include
   --    ("Different_True", Rewriter_Different_True);
   --  Name_To_Rewriter_Map.Include
   --    ("Different_False", Rewriter_Different_False);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_And", Rewrite_De_Morgan_Not_And);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_Or", Rewrite_De_Morgan_Not_Or);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_All_Range", Rewrite_De_Morgan_Not_All_Range);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_All_Elements",
   --     Rewrite_De_Morgan_Not_All_Elements);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_Some_Range", Rewrite_De_Morgan_Not_Some_Range);
   --  Name_To_Rewriter_Map.Include
   --    ("De_Morgan_Not_Some_Elements",
   --     Rewrite_De_Morgan_Not_Some_Elements);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("If_True_Expression",
   --     Rewriter_If_True_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("If_False_Expression",
   --     Rewriter_If_False_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Identical_Expression",
   --     Rewriter_If_Identical_Expression);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("If_Different_Expression", Rewriter_If_Different_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Not_Condition_Expression",
   --     Rewriter_If_Not_Condition_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Not_In_Expression", Rewriter_If_Not_In_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Boolean_If_Condition_Expression",
   --     Rewriter_Boolean_If_Condition_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Boolean_If_Not_Condition_Expression",
   --     Rewriter_Boolean_If_Not_Condition_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Max_Greater_Than",
   --     Rewriter_Integer_Max_Greater_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Max_Greater_Equal",
   --     Rewriter_Integer_Max_Greater_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Max_Less_Than",
   --     Rewriter_Integer_Max_Less_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Max_Less_Equal",
   --     Rewriter_Integer_Max_Less_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Min_Greater_Than",
   --     Rewriter_Integer_Min_Greater_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Min_Greater_Equal",
   --     Rewriter_Integer_Min_Greater_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Min_Less_Than",
   --     Rewriter_Integer_Min_Less_Than);
   --  Name_To_Rewriter_Map.Include
   --    ("Integer_Min_Less_Equal",
   --     Rewriter_Integer_Min_Less_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Concat_Before_If_Expression",
   --     Rewriter_Concat_Before_If_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Concat_After_If_Expression",
   --     Rewriter_Concat_After_If_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Plus_Before_If_Expression",
   --     Rewriter_Plus_Before_If_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Plus_After_If_Expression",
   --     Rewriter_Plus_After_If_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Case_Expression_Binary_With_Others",
   --     Rewriter_Case_Expression_Binary_With_Others);
   --  Name_To_Rewriter_Map.Include ("Double", Rewriter_Double);
   --  Name_To_Rewriter_Map.Include
   --    ("Equals_To_In_Range", Rewriter_Equals_To_In_Range);
   --  Name_To_Rewriter_Map.Include
   --    ("Combine_In_Range_And_Equal",
   --     Rewriter_Combine_In_Range_And_Equal);
   --  Name_To_Rewriter_Map.Include
   --    ("Combine_In_Ranges", Rewriter_Combine_In_Ranges);
   --  Name_To_Rewriter_Map.Include
   --    ("Differents_To_Not_In_Range",
   --     Rewriter_Differents_To_Not_In_Range);
   --  Name_To_Rewriter_Map.Include
   --    ("Combine_Not_In_Range_And_Different",
   --     Rewriter_Combine_Not_In_Range_And_Different);
   --  Name_To_Rewriter_Map.Include
   --    ("Combine_In_Ranges", Rewriter_Combine_In_Ranges);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("Unnecessary_Null_Stmt", Rewriter_Unnecessary_Null_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_True_Stmt", Rewriter_If_True_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_False_Stmt", Rewriter_If_False_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Different_Stmt", Rewriter_If_Different_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Not_Condition_Stmt", Rewriter_If_Not_Condition_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Not_In_Stmt", Rewriter_If_Not_In_Stmt);
   --  Name_To_Rewriter_Map.Include ("Use_Elsif", Rewriter_Use_Elsif);
   --  Name_To_Rewriter_Map.Include
   --    ("Null_Then_Branch", Rewriter_Null_Then_Branch);
   --  Name_To_Rewriter_Map.Include
   --    ("Null_Else_Branch", Rewriter_Null_Else_Branch);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Identical_Branches_Stmt",
   --     Rewriter_If_Identical_Branches_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Identical_Tails_Stmt", Rewriter_If_Identical_Tails_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Argument_Stmt", Rewriter_If_Argument_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Assignment_Stmt", Rewriter_If_Assignment_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Return_Stmt", Rewriter_If_Return_Stmt);
   --  Name_To_Rewriter_Map.Include
   --    ("If_Return_Stmts", Rewriter_If_Return_Stmts);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("Case_Single", Rewriter_Case_Single);
   --  Name_To_Rewriter_Map.Include
   --    ("Case_Binary_With_Others", Rewriter_Case_Binary_With_Others);
   --  Name_To_Rewriter_Map.Include
   --    ("Case_Identical_Branches", Rewriter_Case_Identical_Branches);
   --  Name_To_Rewriter_Map.Include
   --    ("Return_Expression", Rewriter_Return_Expression);
   --  Name_To_Rewriter_Map.Include
   --    ("Declare_And_Overwrite", Rewriter_Declare_And_Overwrite);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Range_And_Then", Rewriter_For_All_Range_And_Then);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Elements_And_Then",
   --     Rewriter_For_All_Elements_And_Then);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Range_Or_Else", Rewriter_For_Some_Range_Or_Else);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Elements_Or_Else",
   --     Rewriter_For_Some_Elements_Or_Else);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Range_Exit", Rewriter_For_All_Range_Exit);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Elements_Exit", Rewriter_For_All_Elements_Exit);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Range_Exit", Rewriter_For_Some_Range_Exit);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Elements_Exit", Rewriter_For_Some_Elements_Exit);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Range_Return", Rewriter_For_All_Range_Return);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Elements_Return", Rewriter_For_All_Elements_Return);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Range_Return", Rewriter_For_Some_Range_Return);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Elements_Return", Rewriter_For_Some_Elements_Return);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Range_All", Rewriter_For_All_Range_All);
   --  Name_To_Rewriter_Map.Include
   --    ("For_All_Elements_All", Rewriter_For_All_Elements_All);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Range_All", Rewriter_For_Some_Range_All);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Some_Elements_All", Rewriter_For_Some_Elements_All);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("Append_To_Unbounded_String",
   --     Rewriter_Append_To_Unbounded_String);
   --  Name_To_Rewriter_Map.Include
   --    ("Append", Rewriter_Append);
   --
   --  Name_To_Rewriter_Map.Include
   --    ("Declarations_Combine", Rewriter_Declarations_Combine);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Attribute_Use", Rewriter_For_Attribute_Use);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Attribute_Use_Aliased",
   --     Rewriter_For_Attribute_Use_Aliased);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Attribute_Use_Array", Rewriter_For_Attribute_Use_Array);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Attribute_Use_Pragma_Var",
   --     Rewriter_For_Attribute_Use_Pragma_Var);
   --  Name_To_Rewriter_Map.Include
   --    ("For_Attribute_Use_Pragma_All",
   --     Rewriter_For_Attribute_Use_Pragma_All);

   Units    : constant Analysis_Units.Vector   := Get_Units;
   Patchers : constant Patchers_Vectors.Vector := Get_Patchers;
begin
   Rewind_Not_Committed_Changes;
   Create_Patches (Units, Patchers);
end Code_Reviewer;
