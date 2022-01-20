with Ada.Containers;              use Ada.Containers;
with Ada.Text_IO;                 use Ada.Text_IO;
with AUnit.Assertions;            use AUnit.Assertions;
with GNAT.Source_Info;            use GNAT.Source_Info;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Shared;                      use Shared;

package body Test_Exercises_Match is

   procedure Test_Rejuvenation_Finder_Public_Subp_Definition_With_3_Parameters
     (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Finder_Public_Subp_Definition_With_3_Parameters
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      function Valid_Node (Node : Ada_Node'Class) return Boolean;
      function Valid_Node (Node : Ada_Node'Class) return Boolean is
      begin
         if Node.Kind = Ada_Subp_Spec then
            declare
               SS : constant Subp_Spec := Node.As_Subp_Spec;
            begin
               return
                 not Inside_Private_Part (SS) and then Is_Part_Of_Subp_Def (SS)
                 and then Nr_Of_Parameters (SS) = 3;
            end;
         else
            return False;
         end if;
      end Valid_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("src/count_subprogram.ads");
      Found_Nodes : constant Node_List.Vector :=
        Find (Unit.Root, Valid_Node'Access);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      for Found_Node of Found_Nodes loop
         declare
            SS : constant Subp_Spec := Found_Node.As_Subp_Spec;
         begin
            Put_Line ("Found " & Image (SS.F_Subp_Name.Text));
         end;
      end loop;
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_Rejuvenation_Finder_Public_Subp_Definition_With_3_Parameters;

   procedure Test_Rejuvenation_Find_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Find_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Unit : constant Analysis_Unit :=
        Analyze_File ("src/assignmentbyifexamples.adb");

      Pattern_Assign_Condition_In_If_Statement : constant Pattern :=
        Make_Pattern
          ("if $S_Condition then $S_Variable := True; " &
           "else $S_Variable := False; end if;",
           If_Stmt_Rule);
      Found_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern_Assign_Condition_In_If_Statement);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Assert
        (Found_Matches.Length = 2,
         "Two instances in unit expected, got " & Found_Matches.Length'Image);
      for Found_Match of Found_Matches loop
         declare
            Condition : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Condition");
            Variable : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Variable");
         begin
            Put_Line
              (Image (Found_Match.Get_Nodes.First_Element.Full_Sloc_Image) &
               Variable & " := " & Condition & ";");
         end;
      end loop;
      Put_Line ("End - " & Enclosing_Entity);
   end Test_Rejuvenation_Find_Assign_Condition_In_If_Statement;

   procedure Test_Rejuvenation_Find_Windows_New_Line
     (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Find_Windows_New_Line
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Handle_Matches (Matches : Match_Pattern_List.Vector);
      procedure Handle_Matches (Matches : Match_Pattern_List.Vector) is
      begin
         for Match of Matches loop
            declare
               Node : constant Ada_Node := Match.Get_Nodes.First_Element;
            begin
               Put_Line (Image (Node.Full_Sloc_Image) & " Found New_Line");
            end;
         end loop;
      end Handle_Matches;

      Unit : constant Analysis_Unit :=
        Analyze_File ("src/newlineexamples.adb");
      Pattern1_New_Line : constant Pattern :=
        Make_Pattern ("ASCII.CR & ASCII.LF", Expr_Rule);
      Found1_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern1_New_Line);

      Pattern2_New_Line : constant Pattern :=
        Make_Pattern ("(1 => ASCII.CR, 2 => ASCII.LF)", Expr_Rule);
      Found2_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern2_New_Line);

      PrefixKey         : constant String  := "$S_prefix";
      Pattern3_New_Line : constant Pattern :=
        Make_Pattern (PrefixKey & " & ASCII.CR & ASCII.LF", Expr_Rule);
      Found3_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern3_New_Line);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Put_Line ("Pattern 1");
      Handle_Matches (Found1_Matches);
      Put_Line ("Pattern 2");
      Handle_Matches (Found2_Matches);
      Put_Line ("Pattern 3");
      Handle_Matches (Found3_Matches);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_Rejuvenation_Find_Windows_New_Line;

   --  TODO: Add real world example usable in open source
   --  procedure Test_Rejuvenation_Find_Modify_Item
   --    (T : in out Test_Case'Class);

   --  Test plumbing

   overriding function Name
     (T : Exercise_Match_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Exercises Match Pattern");
   end Name;

   overriding procedure Register_Tests (T : in out Exercise_Match_Test_Case) is
   begin
      Registration.Register_Routine
        (T,
         Test_Rejuvenation_Finder_Public_Subp_Definition_With_3_Parameters'
           Access,
         "Use Rejuvenation Finder to find Subp_Specs with 3 Parameters");
      Registration.Register_Routine
        (T, Test_Rejuvenation_Find_Assign_Condition_In_If_Statement'Access,
         "Use Rejuvenation to find assign condition in if statements");
      Registration.Register_Routine
        (T, Test_Rejuvenation_Find_Windows_New_Line'Access,
         "Use Rejuvenation to find Windows New Line");
   end Register_Tests;

end Test_Exercises_Match;
