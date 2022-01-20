with Ada.Containers;              use Ada.Containers;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
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
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Default_Value;               use Default_Value;
with Mismatch;                    use Mismatch;
with Prefix_Notation;             use Prefix_Notation;

package body Test_Examples is

   procedure Test_Mismatch (T : in out Test_Case'Class);
   procedure Test_Mismatch (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Condition => 7 = Sum (3, 4), Message => "Sum failed");
   end Test_Mismatch;

   procedure Test_Prefix_Notation (T : in out Test_Case'Class);
   procedure Test_Prefix_Notation (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      My_Var : My_Type;
   begin
      My_Var.Operator_Zero;
      Prefix_Notation.Operator_Zero (My_Var);
      Assert
        (Condition => True, Message => "Prefix notation identical failed");
   end Test_Prefix_Notation;

   procedure Test_Default_Value (T : in out Test_Case'Class);
   procedure Test_Default_Value (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert
        (Condition => My_Function (1) = My_Function (1, 2),
         Message   => "Default function failed");
   end Test_Default_Value;

   procedure Test_String (T : in out Test_Case'Class);
   procedure Test_String (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      --  compiler reports on all conditions
      --  'warning: condition is always True' when using
      --  Expected : constant String := "AB";
      Expected : String (1 .. 2);
   begin
      Expected := "AB";
      Assert (Condition => Expected = ('A', 'B'), Message => "Array");
      Assert
        (Condition => Expected = String'('A', 'B'), Message => "String Array");
      Assert (Condition => Expected = (1 => 'A', 2 => 'B'), Message => "Map");
      Assert
        (Condition => Expected = String'(1 => 'A', 2 => 'B'),
         Message   => "String Map");
      Assert
        (Condition => Expected = (2 => 'B', 1 => 'A'), Message => "Map swap");
      Assert
        (Condition => Expected = String'(2 => 'B', 1 => 'A'),
         Message   => "String Map swap");
      Assert (Condition => Expected = 'A' & 'B', Message => "Concat");
      Assert
        (Condition => Expected = ('A' & 'B'), Message => "Bracketed Concat");
      Assert
        (Condition => Expected = "" & 'A' & 'B', Message => "Empty Concat");
      Assert
        (Condition => Expected = ("" & 'A' & 'B'),
         Message   => "Bracketed Empty Concat");
   end Test_String;

   procedure Test_LibAdaLang_Stmt (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Stmt (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Stmt : constant String        := "x := 42;";
      Unit : constant Analysis_Unit := Analyze_Fragment (Stmt, Stmt_Rule);
   begin
      Put_Line (Stmt);
      Unit.Root.Print;
   end Test_LibAdaLang_Stmt;

   procedure Test_LibAdaLang_Decl (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Decl (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Decl : constant String :=
        "procedure My_Procedure (x,y: Integer; z: String := ""test"");";
      Unit : constant Analysis_Unit := Analyze_Fragment (Decl, Subp_Decl_Rule);
   begin
      Put_Line (Decl);
      Unit.Root.Print;
   end Test_LibAdaLang_Decl;

   procedure Test_Assignment_By_If (T : in out Test_Case'Class);
   procedure Test_Assignment_By_If (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Decl : constant String :=
        "if condition then variable := True; else variable := False; end if;";
      Unit : constant Analysis_Unit := Analyze_Fragment (Decl, If_Stmt_Rule);
   begin
      Put_Line (Decl);
      Unit.Root.Print;
   end Test_Assignment_By_If;

   procedure Test_If_Not (T : in out Test_Case'Class);
   procedure Test_If_Not (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Decl : constant String :=
        "if not condition then handle_not_condition; " &
        "else handle_not_not_condition; end if;";
      Unit : constant Analysis_Unit := Analyze_Fragment (Decl, If_Stmt_Rule);
   begin
      Put_Line (Decl);
      Unit.Root.Print;
   end Test_If_Not;

   procedure Test_LibAdaLang_Visitor (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Visitor (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Ada_Decl_Block =>
               Put_Line ("Skipping Declaration Block");
               return Over;
            when Ada_Object_Decl =>
               declare
                  OD : constant Object_Decl := Node.As_Object_Decl;
               begin
                  Put_Line
                    (Image (OD.Full_Sloc_Image) &
                     "Found Object Decl for Id(s) " & Image (OD.F_Ids.Text));
               end;
               return Into;
            when others =>
               return Into;
         end case;
      end Process_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("tests/" & GNAT.Source_Info.File);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_Visitor;

   function Inside_Decl_Block (Node : Ada_Node'Class) return Boolean;
   function Inside_Decl_Block (Node : Ada_Node'Class) return Boolean is
      Running_Node : Ada_Node := Node.As_Ada_Node;
   begin
      while not Running_Node.Is_Null
        and then Running_Node.Kind /= Ada_Decl_Block
      loop
         Running_Node := Running_Node.Parent;
      end loop;
      return not Running_Node.Is_Null;
   end Inside_Decl_Block;

   procedure Test_Rejuvenation_Find (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Find (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      function Valid_Node (Node : Ada_Node'Class) return Boolean;
      function Valid_Node (Node : Ada_Node'Class) return Boolean is
      begin
         if Node.Kind = Ada_Object_Decl then
            return not Inside_Decl_Block (Node);
         else
            return False;
         end if;
      end Valid_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("tests/" & GNAT.Source_Info.File);
      Found_Nodes : constant Node_List.Vector :=
        Find (Unit.Root, Valid_Node'Access);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      for Found_Node of Found_Nodes loop
         declare
            OD : constant Object_Decl := Found_Node.As_Object_Decl;
         begin
            Put_Line
              (Image (OD.Full_Sloc_Image) & "Found Object Decl for Id(s) " &
               Image (OD.F_Ids.Text));
         end;
      end loop;
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_Rejuvenation_Find;

   procedure Test_Rejuvenation_Match_Pattern (T : in out Test_Case'Class);
   procedure Test_Rejuvenation_Match_Pattern (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Pattern_ObjectDecl_Type_AdaNode_DefaultExpr_Present : constant Pattern :=
        Make_Pattern
          ("$M_vars : Ada_Node := $S_default_expr;", Object_Decl_Rule);

      Unit : constant Analysis_Unit :=
        Analyze_File ("tests/" & GNAT.Source_Info.File);
      Found_Matches : constant Match_Pattern_List.Vector :=
        Find_Full
          (Unit.Root, Pattern_ObjectDecl_Type_AdaNode_DefaultExpr_Present);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      for Found_Match of Found_Matches loop
         declare
            OD : constant Object_Decl :=
              Found_Match.Get_Nodes.First_Element.As_Object_Decl;
            DefaultExpr : constant String :=
              Found_Match.Get_Single_As_Raw_Signature ("$S_default_expr");
            Var_Nodes : constant Node_List.Vector :=
              Found_Match.Get_Multiple_As_Nodes ("$M_vars");
            Vars_String : Unbounded_String;
         begin
            for Var_Node of Var_Nodes loop
               Vars_String := Vars_String & Image (Var_Node.Text) & " ";
            end loop;
            Put_Line
              (Image (OD.Full_Sloc_Image) & "Found Object Decl for Id(s) " &
               To_String (Vars_String) & ": Ada_Node := " & DefaultExpr);
         end;
      end loop;
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_Rejuvenation_Match_Pattern;

   procedure Test_Text_Rewrite (T : in out Test_Case'Class);
   procedure Test_Text_Rewrite (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Func_Begin : constant String :=
        "function Example (a, b : Integer) return Integer is " & "begin " &
        "  return ";
      Argument : constant String := "a+b";
      Func_End : constant String := "; " & "end Example; ";

      Func_Body : constant String :=
        Func_Begin & "Square (" & Argument & ")" & Func_End;

      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Func_Body, Subp_Body_Rule);

      Arg_Key             : constant String  := "$S_arg";
      Pattern_Square_Call : constant Pattern :=
        Make_Pattern ("Square (" & Arg_Key & ")", Expr_Rule);

      Found_Matches : constant Match_Pattern_List.Vector :=
        Find_Full (Unit.Root, Pattern_Square_Call);
      TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Assert
        (Condition => Found_Matches.Length = 1,
         Message   => "One match expected, got " & Found_Matches.Length'Image);
      for Found_Match of Found_Matches loop
         declare
            Node : constant Ada_Node := Found_Match.Get_Nodes.First_Element;
            Arg  : constant String   :=
              Found_Match.Get_Single_As_Raw_Signature (Arg_Key);
         begin
            TR.Replace (Node, "Exponent (Base => " & Arg & ", Power => 2)");
         end;
      end loop;
      Assert
        (Condition => TR.HasReplacements, Message => "Replacements expected");
      Assert
        (Actual   => TR.ApplyToString,
         Expected =>
           Func_Begin & "Exponent (Base => " & Argument & ", Power => 2)" &
           Func_End,
         Message => "Rewrite not as expected");
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_Text_Rewrite;

   procedure Test_Units (T : in out Test_Case'Class);
   procedure Test_Units (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      declare
         Unit : constant Analysis_Unit :=
           Analyze_File_In_Project
             ("src/ParentPackage-ChildPackage.adb",
              "rejuvenation_workshop.gpr");
         CU : constant Compilation_Unit := Unit.Root.As_Compilation_Unit;
      begin
         Assert
           (Condition => CU.P_Unit_Kind = Unit_Body,
            Message   => "*.adb is unexpectedly not a Unit_Body");
         Put_Line ("Withed");
         for WU of CU.P_Withed_Units loop
            Put_Line ("   " & Image (WU.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Withed_Units'Length = 1,
            Message   =>
              "Length of Withed Units is unexpectedly not 1 but " &
              CU.P_Withed_Units'Length'Image);
         Put_Line ("Imported");
         for IU of CU.P_Imported_Units loop
            Put_Line ("   " & Image (IU.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Imported_Units'Length = 2,
            Message   =>
              "Length of Imported Units is unexpectedly not 2 but " &
              CU.P_Imported_Units'Length'Image);
         Put_Line ("Dependencies");
         for UD of CU.P_Unit_Dependencies loop
            Put_Line ("   " & Image (UD.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Unit_Dependencies'Length = 16,
            Message   =>
              "Length of Unit Dependencies is unexpectedly not 16 but " &
              CU.P_Unit_Dependencies'Length'Image);
      end;
      declare
         Unit : constant Analysis_Unit :=
           Analyze_File_In_Project
             ("src/ParentPackage-ChildPackage.ads",
              "rejuvenation_workshop.gpr");
         CU : constant Compilation_Unit := Unit.Root.As_Compilation_Unit;
      begin
         Assert
           (Condition => CU.P_Unit_Kind = Unit_Specification,
            Message   => "*.ads is unexpectedly not a Unit_Specification");
         Put_Line ("Withed");
         for WU of CU.P_Withed_Units loop
            Put_Line ("   " & Image (WU.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Withed_Units'Length = 1,
            Message   =>
              "Length of Withed Units is unexpectedly not 1 but " &
              CU.P_Withed_Units'Length'Image);
         Put_Line ("Imported");
         for IU of CU.P_Imported_Units loop
            Put_Line ("   " & Image (IU.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Imported_Units'Length = 2,
            Message   =>
              "Length of Imported Units is unexpectedly not 2 but " &
              CU.P_Imported_Units'Length'Image);
         Put_Line ("Dependencies");
         for UD of CU.P_Unit_Dependencies loop
            Put_Line ("   " & Image (UD.P_Decl.P_Defining_Name.Text));
         end loop;
         Assert
           (Condition => CU.P_Unit_Dependencies'Length = 14,
            Message   =>
              "Length of Unit Dependencies is unexpectedly not 14 but " &
              CU.P_Unit_Dependencies'Length'Image);
      end;
   end Test_Units;

   --  Test plumbing

   overriding function Name (T : Example_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Workshop Examples");
   end Name;

   overriding procedure Register_Tests (T : in out Example_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Mismatch'Access, "Mismatch");
      Registration.Register_Routine
        (T, Test_Prefix_Notation'Access, "Prefix Notation");
      Registration.Register_Routine
        (T, Test_Default_Value'Access, "Default Value");
      Registration.Register_Routine
        (T, Test_String'Access, "String representations");

      Registration.Register_Routine
        (T, Test_LibAdaLang_Stmt'Access, "LibAdaLang Stmt");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Decl'Access, "LibAdaLang Decl");
      Registration.Register_Routine
        (T, Test_Assignment_By_If'Access, "Assignment by If Statement");
      Registration.Register_Routine
        (T, Test_If_Not'Access,
         "If with not condition - readability issue: double negation");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Visitor'Access,
         "LibAdaLang Visitor for Non-local Declarations");
      Registration.Register_Routine
        (T, Test_Rejuvenation_Find'Access,
         "Rejuvenation Find for Non-local Declarations");
      Registration.Register_Routine
        (T, Test_Rejuvenation_Match_Pattern'Access,
         "Rejuvenation Match Pattern for Object Declarations " &
         "with type Ada_Node and a default expression");
      Registration.Register_Routine
        (T, Test_Text_Rewrite'Access, "Rejuvenation Text Rewrite ");
      Registration.Register_Routine
        (T, Test_Units'Access, "Units - withed / imported");
   end Register_Tests;

end Test_Examples;
