with Ada.Containers;              use Ada.Containers;
with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Missing.AUnit.Assertions;    use Missing.AUnit.Assertions;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

with String_Vectors; use String_Vectors;
with Make_Ada;       use Make_Ada;

package body Test_Match_Patterns_Eagerness is

   procedure Assert is new Generic_Assert (Count_Type);

   --  Test Functions
   Lead_Key : constant String := "$M_Lead";
   Tail_Key : constant String := "$M_Tail";

   procedure Test_Match_Eagerness (T : in out Test_Case'Class);
   procedure Test_Match_Eagerness (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Matching_Element : constant String        := "-1";
      Pattern          : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part =>
                To_Vector (Lead_Key, 1) & Matching_Element & Tail_Key),
           Call_Stmt_Rule);

      Element  : constant String        := "1";
      Lead     : constant Vector        := To_Vector (Element, 3);
      Tail     : constant Vector        := To_Vector (Matching_Element, 4);
      Instance : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part => Lead & Matching_Element & Tail),
           Call_Stmt_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
      Actual_Lead : constant Node_List.Vector :=
        MP.Get_Multiple_As_Nodes (Lead_Key);
      Actual_Tail : constant Node_List.Vector :=
        MP.Get_Multiple_As_Nodes (Tail_Key);
   begin
      Assert
        (Condition => not Lead.Contains (Matching_Element),
         Message   =>
           "Precondition violated: " & "lead contains matching element");
      Assert
        (Condition => Tail.Contains (Matching_Element),
         Message   =>
           "Precondition violated: " &
           "tail does not contain matching element");
      Assert (Condition => Actual, Message => "Match expected");
      Assert
        (Expected => Lead.Length, Actual => Actual_Lead.Length,
         Message  => "Different sizes of Lead");
      Assert
        (Expected => Tail.Length, Actual => Actual_Tail.Length,
         Message  => "Different sizes of Tail");
   end Test_Match_Eagerness;

   procedure Test_Adjacent_Placeholders_Eagerness (T : in out Test_Case'Class);
   procedure Test_Adjacent_Placeholders_Eagerness (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Pattern : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part => Lead_Key & Tail_Key),
           Call_Stmt_Rule);

      Element               : constant String        := "1";
      Actual_Parameter_Part : constant Vector        := To_Vector (Element, 5);
      Instance              : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part => Actual_Parameter_Part),
           Call_Stmt_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
      Actual_Lead : constant Node_List.Vector :=
        MP.Get_Multiple_As_Nodes (Lead_Key);
      Actual_Tail : constant Node_List.Vector :=
        MP.Get_Multiple_As_Nodes (Tail_Key);
   begin
      Assert (Condition => Actual, Message => "Match expected");
      Assert
        (Expected => 0, Actual => Actual_Lead.Length,
         Message  => "Different sizes of Lead.");
      Assert
        (Expected => Actual_Parameter_Part.Length,
         Actual => Actual_Tail.Length, Message => "Different sizes of Tail.");
   end Test_Adjacent_Placeholders_Eagerness;

   procedure Test_Limited_Match (T : in out Test_Case'Class);
   procedure Test_Limited_Match (T : in out Test_Case'Class) is
      --  Match is possible for $M_Lead, 1, 1 in instance 1, 2, 1, 1
      --  However that would require backtracking,
      --  which is currently not implemented / supported

      pragma Unreferenced (T);

      Element                          : constant String := "1";
      Matching_Sequence : constant Vector := To_Vector (Element, 2);
      Another_Element                  : constant String := "2";
      Only_Initially_Matching_Sequence : constant Vector :=
        Element & Another_Element;

      Pattern : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part => Lead_Key & Matching_Sequence),
           Call_Stmt_Rule);

      Instance : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part =>
                Only_Initially_Matching_Sequence & Matching_Sequence),
           Call_Stmt_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Matching_Sequence.First_Element =
         Only_Initially_Matching_Sequence.First_Element,
         "Initial match expected");
      Assert
        (Matching_Sequence.Last_Element /=
         Only_Initially_Matching_Sequence.Last_Element,
         "Mismatch expected");

      Assert (Condition => not Actual, Message => "Mismatch expected");
      Assert
        (Expected => 0, Actual => MP.Get_Nodes.Length,
         Message  => "No match implies no nodes");
   end Test_Limited_Match;

   procedure Test_Double_Match (T : in out Test_Case'Class);
   procedure Test_Double_Match (T : in out Test_Case'Class) is
      --  Match is possible for $M_Lead, $M_Lead in
      --     1, 1
      --     1, 2, 1, 2
      --  However that would require functionality,
      --  which is currently not implemented / supported

      pragma Unreferenced (T);

      Pattern : constant Analysis_Unit :=
        Analyze_Fragment
          (Make_Procedure_Call_Statement
             (Actual_Parameter_Part => Lead_Key & Lead_Key),
           Call_Stmt_Rule);
   begin
      declare
         Element           : constant String        := "1";
         Matching_Sequence : constant Vector        := To_Vector (Element, 1);
         Instance          : constant Analysis_Unit :=
           Analyze_Fragment
             (Make_Procedure_Call_Statement
                (Actual_Parameter_Part =>
                   Matching_Sequence & Matching_Sequence),
              Call_Stmt_Rule);

         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance.Root);
      begin
         Assert
           (Condition => not Actual, Message => "No match expected for 1,1");
         Assert
           (Expected => 0, Actual => MP.Get_Nodes.Length,
            Message  => "No match implies no nodes");
      end;

      declare
         Element           : constant String        := "1";
         Another_Element   : constant String        := "2";
         Matching_Sequence : constant Vector := Element & Another_Element;
         Instance          : constant Analysis_Unit :=
           Analyze_Fragment
             (Make_Procedure_Call_Statement
                (Actual_Parameter_Part =>
                   Matching_Sequence & Matching_Sequence),
              Call_Stmt_Rule);

         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance.Root);
      begin
         Assert
           (Condition => not Actual,
            Message   => "No match expected for 1, 2, 1, 2");
         Assert
           (Expected => 0, Actual => MP.Get_Nodes.Length,
            Message  => "No match implies no nodes");
      end;
   end Test_Double_Match;

   procedure Test_Empty_List_Only (T : in out Test_Case'Class);
   procedure Test_Empty_List_Only (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Pattern : constant Analysis_Unit :=
        Analyze_Fragment
          ("if $S_Cond then $S_F ($M_Before, $S_True, $M_After);" &
           "else $S_F ($M_Before, $S_False, $M_After);" & "end if;",
           If_Stmt_Rule);
   begin
      declare
         Instance_Match : constant Analysis_Unit :=
           Analyze_Fragment
             ("if Cond then My_F (X, 1, 2, 3);" & "else My_F (Y, 1, 2, 3);" &
              "end if;",
              If_Stmt_Rule);

         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance_Match.Root);
      begin
         Assert
           (Condition => Actual,
            Message   => "Match expected for Instance_Match");
         Assert
           (Condition => not MP.Get_Nodes.Is_Empty,
            Message   => "Match implies nodes");
      end;

      declare
         Instance_No_Match : constant Analysis_Unit :=
           Analyze_Fragment
             ("if Cond then My_F (1, 2, 3, X);" & "else My_F (1, 2, 3, Y);" &
              "end if;",
              If_Stmt_Rule);
         --  Correct Instance, yet no match
         --  due to limitations of current implementation.

         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance_No_Match.Root);
      begin
         Assert
           (Condition => not Actual,
            Message   => "No match expected for Instance_No_Match");
         Assert
           (Expected => 0, Actual => MP.Get_Nodes.Length,
            Message  => "No match implies no nodes");
      end;
   end Test_Empty_List_Only;

   --  Test plumbing

   overriding function Name
     (T : Match_Patterns_Eagerness_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Match Pattern Eagerness");
   end Name;

   overriding procedure Register_Tests
     (T : in out Match_Patterns_Eagerness_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Match_Eagerness'Access, "Match Eagerness");
      Registration.Register_Routine
        (T, Test_Adjacent_Placeholders_Eagerness'Access,
         "Adjacent Placeholders Eagerness");
      Registration.Register_Routine
        (T, Test_Limited_Match'Access,
         "Limited match capabilities due to eagerness");
      Registration.Register_Routine
        (T, Test_Double_Match'Access, "Double match capabilities");
      Registration.Register_Routine
        (T, Test_Empty_List_Only'Access, "Empty List only match");
   end Register_Tests;

end Test_Match_Patterns_Eagerness;
