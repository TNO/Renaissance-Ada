with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Containers;              use Ada.Containers;
with AUnit.Assertions;            use AUnit.Assertions;
with GNAT.Source_Info;            use GNAT.Source_Info;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;

package body Test_Exercises_Rewrite is

   procedure Test_Rejuvenation_Rewrite_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Rewrite_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Function_Body_Start : constant String :=
        "function Example (condition : in Boolean) return Boolean is" &
        "   returnValue : Boolean;" & "begin";
      Function_Body_End : constant String :=
        "   return returnValue;" & "end Example;";
      Function_Body : constant String :=
        Function_Body_Start & "   if condition then" &
        "      returnValue := True;" & "   else" &
        "      returnValue := False;" & "   end if;" & Function_Body_End;

      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Function_Body, Subp_Body_Rule);

      Expected : constant String :=
        Function_Body_Start & "   returnValue := condition;" &
        Function_Body_End;

      Pattern_Assign_Condition_In_If_Statement : constant Pattern :=
        Make_Pattern
          ("if $S_Condition then $S_Variable := True; " &
           "else $S_Variable := False; end if;",
           Stmt_Rule);
      Found_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern_Assign_Condition_In_If_Statement);
      TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Assert
        (Found_Matches.Length = 1,
         "One instance in unit expected, got " & Found_Matches.Length'Image);
      for Found_Match of Found_Matches loop
         declare
            Condition : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Condition");
            Variable : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Variable");
         begin
            TR.Replace
              (Found_Match.Get_Nodes.First_Element,
               Variable & " := " & Condition & ";");
         end;
      end loop;
      declare
         Actual : constant String := TR.ApplyToString;
      begin
         Assert
           (Actual = Expected,
            "* Expected :" & ASCII.LF & Expected & ASCII.LF & "* Actual :" &
            ASCII.LF & Actual);
      end;
      Put_Line ("End - " & Enclosing_Entity);
   end Test_Rejuvenation_Rewrite_Assign_Condition_In_If_Statement;

   procedure Test_Rejuvenation_Rewrite_Assign_Condition_In_Statement_List
     (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Rewrite_Assign_Condition_In_Statement_List
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Function_Body_Start : constant String :=
        "function Example (boolCond : in Boolean) return Boolean is" &
        "   answer : Boolean;" & "begin";
      Function_Body_End : constant String :=
        "   return answer;" & "end Example;";
      Function_Body : constant String :=
        Function_Body_Start & "   answer := False;" & "   if boolCond then" &
        "      answer := True;" & "   end if;" & Function_Body_End;

      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Function_Body, Subp_Body_Rule);

      Expected : constant String :=
        Function_Body_Start & "   answer := boolCond;" & Function_Body_End;

      Pattern_Assign_Condition_In_Statement_List : constant Pattern :=
        Make_Pattern
          ("$S_Variable := False; " &
           "if $S_Condition then $S_Variable := True; end if;",
           Stmts_Rule);
      Found_Matches : constant Match_Pattern_List.Vector :=
        Find_Sub_List (Unit.Root, Pattern_Assign_Condition_In_Statement_List);
      TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Assert
        (Found_Matches.Length = 1,
         "One instance in unit expected, got " & Found_Matches.Length'Image);
      for Found_Match of Found_Matches loop
         declare
            Condition : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Condition");
            Variable : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_Variable");
         begin
            TR.Replace
              (Found_Match.Get_Nodes.First_Element,
               Found_Match.Get_Nodes.Last_Element,
               Variable & " := " & Condition & ";");
         end;
      end loop;
      declare
         Actual : constant String := TR.ApplyToString;
      begin
         Assert
           (Actual = Expected,
            "* Expected :" & ASCII.LF & Expected & ASCII.LF & "* Actual :" &
            ASCII.LF & Actual);
      end;
      Put_Line ("End - " & Enclosing_Entity);
   end Test_Rejuvenation_Rewrite_Assign_Condition_In_Statement_List;

   --  Test plumbing

   overriding function Name
     (T : Exercise_Rewrite_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Exercises Rewrite");
   end Name;

   overriding procedure Register_Tests (T : in out Exercise_Rewrite_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Rejuvenation_Rewrite_Assign_Condition_In_If_Statement'Access,
         "Use Rejuvenation Match Pattern to " &
         "find assignment of condition to variable " &
         "using if statement. Pattern 1");
      Registration.Register_Routine
        (T,
         Test_Rejuvenation_Rewrite_Assign_Condition_In_Statement_List'Access,
         "Use Rejuvenation Match Pattern to " &
         "find assignment of condition to variable " &
         "using statements. Pattern 2");
   end Register_Tests;

end Test_Exercises_Rewrite;
