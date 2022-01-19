with Ada.Containers;              use Ada.Containers;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with AUnit.Assertions;            use AUnit.Assertions;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Missing.AUnit.Assertions;    use Missing.AUnit.Assertions;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;

with String_Vectors; use String_Vectors;
with Make_Ada;       use Make_Ada;

package body Test_Match_Patterns_Placeholders is

   procedure Assert is new Generic_Assert (Count_Type);
   procedure Assert is new Generic_Assert (Ada_Node_Kind_Type);

   procedure Test_Placeholder_Slice (T : in out Test_Case'Class);
   procedure Test_Placeholder_Slice (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      VarName  : constant String        := "My_Var";
      Instance : constant Analysis_Unit :=
        Analyze_Fragment (VarName, Expr_Rule);

      procedure TestCase_Single (InsertKey, RetrieveKey : String);
      procedure TestCase_Single (InsertKey, RetrieveKey : String) is
         Pattern : constant Analysis_Unit :=
           Analyze_Fragment (InsertKey, Expr_Rule);
         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance.Root);
      begin
         Assert (Condition => Actual, Message => "Match expected");
         Assert
           (Actual   => MP.Get_Single_As_Raw_Signature (RetrieveKey),
            Expected => VarName, Message => "Value of Placeholder differ");
      end TestCase_Single;

      Key       : constant String := "$S_Var";
      Twice     : constant String := 2 * Key;
      KeySlice1 : constant String :=
        Twice (Twice'First .. Twice'First + Key'Length - 1);
      KeySlice2 : constant String :=
        Twice (Twice'First + Key'Length .. Twice'Last);
   begin
      Assert
        (Condition => KeySlice1 = KeySlice2,
         Message   => "KeySlices unexpectedly differ");
      Assert
        (Condition => KeySlice1'First /= KeySlice2'First,
         Message   =>
           "First index of Keyslices unexpectedly does not differ " &
           KeySlice2'First'Image);

      TestCase_Single (KeySlice1, KeySlice2);
      TestCase_Single (KeySlice2, KeySlice1);
   end Test_Placeholder_Slice;

   procedure Test_Match_Single_Kind (T : in out Test_Case'Class);
   procedure Test_Match_Single_Kind (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Match_Single_Kind (Prefix : Vector);
      procedure Test_Match_Single_Kind (Prefix : Vector) is
         Placeholder_Name : constant String := "$S_Name";
         Pattern_Str      : constant String :=
           Make_Object_Declaration_Subtype_Indication
             (Defining_Identifier_List => Prefix & Placeholder_Name);
         Pattern : constant Analysis_Unit :=
           Analyze_Fragment (Pattern_Str, Basic_Decl_Rule);

         Instance_Str : constant String :=
           Make_Object_Declaration_Subtype_Indication
             (Defining_Identifier_List => Prefix & "X");
         Instance : constant Analysis_Unit :=
           Analyze_Fragment (Instance_Str, Basic_Decl_Rule);

         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance.Root);
      begin
         Assert
           (Condition => Actual, Message => "Instance doesn't match pattern.");
         declare
            Nodes : constant Node_List.Vector :=
              MP.Get_Placeholder_As_Nodes (Placeholder_Name);
         begin
            Assert
              (Actual  => Nodes.Length, Expected => 1,
               Message => "Nodes Length differ.");
            for Node of Nodes loop
               Assert
                 (Actual  => Node.Kind, Expected => Ada_Defining_Name,
                  Message => "Node kind differ for " & Raw_Signature (Node));
            end loop;
         end;
      end Test_Match_Single_Kind;

   begin
      Test_Match_Single_Kind (Empty_Vector);
      Test_Match_Single_Kind (To_Vector ("Prefix", 1));
   end Test_Match_Single_Kind;

   procedure Test_Match_Multiple_Kind (T : in out Test_Case'Class);
   procedure Test_Match_Multiple_Kind (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Match_Multiple_Kind (Prefix : Vector);
      procedure Test_Match_Multiple_Kind (Prefix : Vector) is
         Placeholder_Name : constant String := "$M_Name";
         Pattern_Str      : constant String :=
           Make_Object_Declaration_Subtype_Indication
             (Defining_Identifier_List => Prefix & Placeholder_Name);
         Pattern : constant Analysis_Unit :=
           Analyze_Fragment (Pattern_Str, Basic_Decl_Rule);

         type VectorArray is array (Natural range <>) of Vector;
         All_Defining_Name_Lists : constant VectorArray :=
           (Empty_Vector, To_Vector ("X", 1), "A" & "B",
            To_Vector ("a", 1) & "b" & "c" & "d" & "e" & "f");

         Defining_Name_Lists : constant VectorArray :=
           All_Defining_Name_Lists
             (All_Defining_Name_Lists'First +
                (if Prefix.Is_Empty then 1 else 0) ..
                  All_Defining_Name_Lists'Last);
      begin
         for Defining_Name_List of Defining_Name_Lists loop
            declare
               Instance_Str : constant String :=
                 Make_Object_Declaration_Subtype_Indication
                   (Defining_Identifier_List => Prefix & Defining_Name_List);
               Instance : constant Analysis_Unit :=
                 Analyze_Fragment (Instance_Str, Basic_Decl_Rule);

               MP     : Match_Pattern;
               Actual : constant Boolean :=
                 Match_Full (MP, Pattern.Root, Instance.Root);
            begin
               Assert
                 (Condition => Actual,
                  Message   => "Instance doesn't match pattern.");
               declare
                  Nodes : constant Node_List.Vector :=
                    MP.Get_Placeholder_As_Nodes (Placeholder_Name);
               begin
                  Assert
                    (Actual   => Nodes.Length,
                     Expected => Defining_Name_List.Length,
                     Message  => "Nodes Length differ");
                  for Node of Nodes loop
                     Assert
                       (Actual  => Node.Kind, Expected => Ada_Defining_Name,
                        Message =>
                          "Node kind differ for " & Raw_Signature (Node));
                  end loop;
               end;
            end;
         end loop;
      end Test_Match_Multiple_Kind;
   begin
      Test_Match_Multiple_Kind (Empty_Vector);
      Test_Match_Multiple_Kind (To_Vector ("Prefix", 1));
   end Test_Match_Multiple_Kind;

   procedure Test_Placeholder_In_CaseStmtAlternativeList
     (T : in out Test_Case'Class);
   procedure Test_Placeholder_In_CaseStmtAlternativeList
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Find_Pattern : constant Pattern :=
        Make_Pattern
          ("case $S_Expr is " & "when '*'| $M_Values => $M_Stmts_True;" &
           "when others => $M_Stmts_False;" & "end case;",
           Case_Stmt_Rule);
      Unit : constant Analysis_Unit :=
        Analyze_Fragment
          ("case c is " & "when '*' | '?' | '+' => in_call;" &
           "when others => out_call;" & "end case;",
           Case_Stmt_Rule);
      Expected : constant String  := "'?' | '+'";
      MP       : Match_Pattern;
      Actual   : constant Boolean :=
        Match_Full (MP, Find_Pattern.As_Ada_Node, Unit.Root);
   begin
      Assert
        (Condition => Actual,
         Message   =>
           "Instance doesn't match pattern unexpectedly." & ASCII.CR &
           ASCII.LF & "Instance = " & Image (Unit.Text) & ASCII.CR & ASCII.LF &
           "Pattern = " & Find_Pattern.Get_String & ASCII.CR & ASCII.LF);
      Assert
        (Expected => Expected,
         Actual   => MP.Get_Placeholder_As_Raw_Signature ("$M_Values"),
         Message  => "Placeholder raw signature is not as expected");
   end Test_Placeholder_In_CaseStmtAlternativeList;

   --  Test plumbing

   overriding function Name
     (T : Match_Patterns_Placeholders_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Match Pattern Placeholders");
   end Name;

   overriding procedure Register_Tests
     (T : in out Match_Patterns_Placeholders_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Placeholder_Slice'Access, "Placeholder slice");
      Registration.Register_Routine
        (T, Test_Match_Single_Kind'Access, "Match Single kind");
      Registration.Register_Routine
        (T, Test_Match_Multiple_Kind'Access, "Match Multiple kind");
      Registration.Register_Routine
        (T, Test_Placeholder_In_CaseStmtAlternativeList'Access,
         "Placeholder Case Stmt Alternative List");
   end Register_Tests;

end Test_Match_Patterns_Placeholders;
