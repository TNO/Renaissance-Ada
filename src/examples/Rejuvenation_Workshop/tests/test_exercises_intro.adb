with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;                 use Ada.Text_IO;
with GNAT.Source_Info;            use GNAT.Source_Info;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Test_Exercises_Intro is

   procedure Test_LibAdaLang_AST (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_AST (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Unit : constant Analysis_Unit := Analyze_File ("src/mismatch.ads");
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Print;
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_AST;

   procedure Test_LibAdaLang_Subprograms (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Subprograms (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Subp_Body then
            declare
               SB : constant Subp_Body := Node.As_Subp_Body;
            begin
               Put_Line ("Found " & Image (SB.F_Subp_Spec.F_Subp_Name.Text));
            end;
         end if;
         return Into;
      end Process_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("tests/" & GNAT.Source_Info.File);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_Subprograms;

   procedure Test_LibAdaLang_CallFunction (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_CallFunction (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Function_Name : constant String := "Analyze_File";

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Call_Expr then
            declare
               CE : constant Call_Expr := Node.As_Call_Expr;
            begin
               if Ada.Strings.Equal_Case_Insensitive
                   (Image (CE.F_Name.Text), Function_Name)
               then
                  Put_Line
                    (Image (CE.Full_Sloc_Image) & "Call to '" & Function_Name &
                     "'");
               end if;
            end;
         end if;
         return Into;
      end Process_Node;

      Project_Filename : constant String                := "test_driver.gpr";
      Units            : constant Analysis_Units.Vector :=
        Analyze_Project (Project_Filename);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      for Unit of Units loop
         Unit.Root.Traverse (Process_Node'Access);
      end loop;
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_CallFunction;

   --  Test plumbing

   overriding function Name
     (T : Exercise_Intro_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Exercises Introduction");
   end Name;

   overriding procedure Register_Tests (T : in out Exercise_Intro_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_LibAdaLang_AST'Access, "Use LibAdaLang to print AST of file");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Subprograms'Access,
         "Use LibAdaLang to print subprograms in file");
      Registration.Register_Routine
        (T, Test_LibAdaLang_CallFunction'Access,
         "Use LibAdaLang to find all calls " &
         "to a particular function in project");
   end Register_Tests;

end Test_Exercises_Intro;
