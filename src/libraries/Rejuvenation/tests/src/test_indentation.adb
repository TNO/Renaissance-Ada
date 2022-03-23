with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Missing.AUnit.Assertions;    use Missing.AUnit.Assertions;
with Rejuvenation.Indentation;    use Rejuvenation.Indentation;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

with Make_Ada; use Make_Ada;

package body Test_Indentation is

   procedure Assert is new Generic_Assert (Integer);

   --  Test Functions
   procedure Test_No_Indentation (T : in out Test_Case'Class);
   procedure Test_No_Indentation (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      for Index in 0 .. 10 loop
         declare
            Actual_Spacing : constant String        := Index * " ";
            Instance       : constant Analysis_Unit :=
              Analyze_Fragment
                (Make_Procedure_Call_Statement & Actual_Spacing &
                 Make_Procedure_Call_Statement,
                 Stmts_Rule);
         begin
            Assert
              (Expected => 2, Actual => Instance.Root.Children_Count,
               Message  => "Two statements expected");
            Assert
              (Expected => No_Indentation,
               Actual   =>
                 Indentation_Of_Node
                   (Instance.Root.Child (Instance.Root.Last_Child_Index)),
               Message => "Mismatch at " & Index'Image);
         end;
      end loop;
   end Test_No_Indentation;

   procedure Test_Indentation (T : in out Test_Case'Class);
   procedure Test_Indentation (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      for Index in 0 .. 10 loop
         declare
            Actual_Indentation : constant String        := Index * " ";
            Instance           : constant Analysis_Unit :=
              Analyze_Fragment
                (ASCII.LF & Actual_Indentation & Make_Procedure_Call_Statement,
                 Call_Stmt_Rule);
         begin
            Assert
              (Expected => Index,
               Actual   => Indentation_Of_Node (Instance.Root),
               Message  => "Mismatch at " & Index'Image);
         end;
      end loop;
   end Test_Indentation;

   procedure Test_Initial_Indentation (T : in out Test_Case'Class);
   procedure Test_Initial_Indentation (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      for Index in 0 .. 10 loop
         declare
            Actual_Indentation : constant String        := Index * " ";
            Instance           : constant Analysis_Unit :=
              Analyze_Fragment
                (Actual_Indentation & Make_Procedure_Call_Statement,
                 Call_Stmt_Rule);
         begin
            Assert
              (Expected => Index,
               Actual   => Indentation_Of_Node (Instance.Root),
               Message  => "Mismatch at " & Index'Image);
         end;
      end loop;
   end Test_Initial_Indentation;

   procedure Test_Root_On_Separate_Lines (T : in out Test_Case'Class);
   procedure Test_Root_On_Separate_Lines (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Make_Procedure_Call_Statement, Call_Stmt_Rule);
   begin
      Assert
        (Condition => Node_On_Separate_Lines (Instance.Root),
         Message   => "Expect that root is on separate lines");
   end Test_Root_On_Separate_Lines;

   --  Test plumbing

   overriding function Name
     (T : Indentation_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Indentation");
   end Name;

   overriding procedure Register_Tests (T : in out Indentation_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_No_Indentation'Access,
         "No Indentation - Earlier node on same line");
      Registration.Register_Routine
        (T, Test_Indentation'Access, "Indentation");
      Registration.Register_Routine
        (T, Test_Initial_Indentation'Access,
         "Initial Indentation - Node on first line / at beginning of text");
      Registration.Register_Routine
        (T, Test_Root_On_Separate_Lines'Access, "Root on separate lines");
   end Register_Tests;

end Test_Indentation;
