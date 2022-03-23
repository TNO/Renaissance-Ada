with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Replacer;       use Rejuvenation.Replacer;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with String_Maps;                 use String_Maps;
with String_Vectors;              use String_Vectors;

with Make_Ada;   use Make_Ada;
with Assert_AST; use Assert_AST;

package body Test_Replacer is

   --  Test Functions

   procedure Test_No_Replacement (T : in out Test_Case'Class);
   procedure Test_No_Replacement (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule     : constant Grammar_Rule := Object_Decl_Rule;
      Expected : constant String := Make_Object_Declaration_Subtype_Indication;

      Unit   : constant Analysis_Unit := Analyze_Fragment (Expected, Rule);
      Actual : constant String        := Replace (Unit.Root, Empty_Map);
   begin
      Assert_Equal_AST
        (Expected, Actual, Rule, "No placeholders with empty replacment map");
   end Test_No_Replacement;

   procedure Test_Placeholder_Replacements (T : in out Test_Case'Class);
   procedure Test_Placeholder_Replacements (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name : constant String       := "$M_BlaBla";
      Replacement      : constant String       := "My_Name";
      Input            : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Defining_Identifier_List => To_Vector (Placeholder_Name, 1));
      Expected : constant String :=
        Make_Object_Declaration_Subtype_Indication
          (Defining_Identifier_List => To_Vector (Replacement, 1));

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            "Simple replacement: placeholder with non-empty value");
      end;
   end Test_Placeholder_Replacements;

   procedure Test_Object_Decl_No_Defining_Names (T : in out Test_Case'Class);
   procedure Test_Object_Decl_No_Defining_Names (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name : constant String       := "$M_BlaBla";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Defining_Identifier_List => To_Vector (Placeholder_Name, 1));
      Expected : constant String := "";

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert
           (Actual  => Actual, Expected => Expected,
            Message =>
              "When an object Declaration has no defining name, " &
              "it should be removed.");
      end;
   end Test_Object_Decl_No_Defining_Names;

   procedure Test_Object_Decl_No_Default_Expr (T : in out Test_Case'Class);
   procedure Test_Object_Decl_No_Default_Expr (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name : constant String       := "$M_BlaBla";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Expression => Placeholder_Name);
      Expected : constant String := Make_Object_Declaration_Subtype_Indication;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            Message =>
              "When an object Declaration has no default expression," &
              "the ':=' should be removed.");
      end;
   end Test_Object_Decl_No_Default_Expr;

   procedure Test_Object_Decl_No_Aspects (T : in out Test_Case'Class);
   procedure Test_Object_Decl_No_Aspects (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name : constant String       := "$M_Aspects";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Aspect_List => To_Vector (Placeholder_Name, 1));
      Expected : constant String := Make_Object_Declaration_Subtype_Indication;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            Message =>
              "When an object Declaration has no aspects," &
              "the 'with' keyword should be absent.");
      end;
   end Test_Object_Decl_No_Aspects;

   procedure Test_Object_Decl_Empty_Aspects (T : in out Test_Case'Class);
   procedure Test_Object_Decl_Empty_Aspects (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule               : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name_1 : constant String       := "$M_Aspects_1";
      Replacement_1      : constant String       := "";
      Placeholder_Name_2 : constant String       := "$M_Aspects_2";
      Replacement_2      : constant String       := "";
      Input              : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Aspect_List => Placeholder_Name_1 & Placeholder_Name_2);
      Expected : constant String := Make_Object_Declaration_Subtype_Indication;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name_1, Replacement_1);
      Replacements.Insert (Placeholder_Name_2, Replacement_2);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            Message =>
              "When an object Declaration has empty aspects," &
              "the 'with' keyword should be absent.");
      end;
   end Test_Object_Decl_Empty_Aspects;

   procedure Test_Object_Decl_Empty_Head_Aspects (T : in out Test_Case'Class);
   procedure Test_Object_Decl_Empty_Head_Aspects (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Object_Decl_Rule;
      Placeholder_Name : constant String       := "$M_Aspects";
      Replacement      : constant String       := "";
      Other_Aspects    : constant Vector := To_Vector ("Unreferenced", 1);
      Input            : constant String       :=
        Make_Object_Declaration_Subtype_Indication
          (Aspect_List => Placeholder_Name & Other_Aspects);
      Expected : constant String :=
        Make_Object_Declaration_Subtype_Indication
          (Aspect_List => Other_Aspects);

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            Message =>
              "When an object Declaration has empty head in aspects," &
              "the 'with' keyword should be present, " &
              "yet the separator ',' must be absent.");
      end;
   end Test_Object_Decl_Empty_Head_Aspects;

   procedure Test_Empty_Stmt_List (T : in out Test_Case'Class);
   procedure Test_Empty_Stmt_List (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Stmts_Rule;
      Placeholder_Name : constant String       := "$M_BlaBla";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Procedure_Call_Statement (Procedure_Name => Placeholder_Name);
      Expected : constant String := Make_Null_Statement;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST (Expected, Actual, Rule, "Empty Stmt List: null;");
      end;
   end Test_Empty_Stmt_List;

   procedure Test_Empty_Param_Assoc (T : in out Test_Case'Class);
   procedure Test_Empty_Param_Assoc (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule          := Expr_Rule;
      Placeholder_Name : constant String                := "$M_12DD_";
      Replacement      : constant String                := "";
      Prefix           : constant String_Vectors.Vector := "3" & "True";
      Input            : constant String                :=
        Make_Function_Call
          (Actual_Parameter_Part => Prefix & Placeholder_Name);
      Expected : constant String :=
        Make_Function_Call (Actual_Parameter_Part => Prefix);

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule, "Empty Param Assoc: no separator");
      end;
   end Test_Empty_Param_Assoc;

   procedure Test_Param_Assoc_Empty_Name (T : in out Test_Case'Class);
   procedure Test_Param_Assoc_Empty_Name (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Expr_Rule;
      Placeholder_Name : constant String       := "$M_1";
      Replacement      : constant String       := "";
      Value            : constant String       := "3";
      Input            : constant String       :=
        Make_Function_Call
          (Actual_Parameter_Part =>
             To_Vector
               (Make_Parameter_Association (Placeholder_Name, Value), 1));
      Expected : constant String :=
        Make_Function_Call
          (Actual_Parameter_Part =>
             To_Vector (Make_Parameter_Association (Value => Value), 1));

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST (Expected, Actual, Rule, "Param Assoc: Empty Name");
      end;
   end Test_Param_Assoc_Empty_Name;

   procedure Test_Call_Expr_No_Arguments (T : in out Test_Case'Class);
   procedure Test_Call_Expr_No_Arguments (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Expr_Rule;
      Placeholder_Name : constant String       := "$M_e";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Function_Call
          (Actual_Parameter_Part => To_Vector (Placeholder_Name, 1));
      Expected : constant String := Make_Function_Call;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            "Call Expr with Empty Argument List: no brackets");
      end;
   end Test_Call_Expr_No_Arguments;

   procedure Test_If_Stmt_Empty_Else (T : in out Test_Case'Class);
   procedure Test_If_Stmt_Empty_Else (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := If_Stmt_Rule;
      Placeholder_Name : constant String       := "$M_Stmts";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_If_Statement
          (Stmts_False =>
             Make_Procedure_Call_Statement
               (Procedure_Name => Placeholder_Name));
      Expected : constant String := Make_If_Statement;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            "If statement with Empty else branch: no 'else' keyword.");
      end;
   end Test_If_Stmt_Empty_Else;

   procedure Test_Block_Stmt_Empty_Declarative_Part
     (T : in out Test_Case'Class);
   procedure Test_Block_Stmt_Empty_Declarative_Part
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Rule             : constant Grammar_Rule := Block_Stmt_Rule;
      Placeholder_Name : constant String       := "$M_Ids";
      Replacement      : constant String       := "";
      Input            : constant String       :=
        Make_Block_Statement
          (Declarative_Part =>
             Make_Object_Declaration_Subtype_Indication
               (Defining_Identifier_List => To_Vector (Placeholder_Name, 1)));
      Expected : constant String := Make_Block_Statement;

      Unit         : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      Replacements : Map;
   begin
      Replacements.Insert (Placeholder_Name, Replacement);
      declare
         Actual : constant String := Replace (Unit.Root, Replacements);
      begin
         Assert_Equal_AST
           (Expected, Actual, Rule,
            "Block statement with Empty Declarative Part: " &
            "no 'declare' keyword.");
      end;
   end Test_Block_Stmt_Empty_Declarative_Part;

   --  Test plumbing

   overriding function Name
     (T : Replacer_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Replacer");
   end Name;

   overriding procedure Register_Tests (T : in out Replacer_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_No_Replacement'Access, "No replacement");
      Registration.Register_Routine
        (T, Test_Placeholder_Replacements'Access, "Simple replacements");
      Registration.Register_Routine
        (T, Test_Object_Decl_No_Defining_Names'Access,
         "Object decl with no defining names");
      Registration.Register_Routine
        (T, Test_Object_Decl_No_Default_Expr'Access,
         "Object decl with no default expression");
      Registration.Register_Routine
        (T, Test_Object_Decl_No_Aspects'Access, "Object decl with no aspects");
      Registration.Register_Routine
        (T, Test_Object_Decl_Empty_Aspects'Access,
         "Object decl with multiple empty aspects");
      Registration.Register_Routine
        (T, Test_Object_Decl_Empty_Head_Aspects'Access,
         "Object decl with empty head of aspects");
      Registration.Register_Routine
        (T, Test_Empty_Stmt_List'Access, "Empty Statement List");
      Registration.Register_Routine
        (T, Test_Empty_Param_Assoc'Access, "Empty Param Assoc");
      Registration.Register_Routine
        (T, Test_Param_Assoc_Empty_Name'Access,
         "Empty Param Name in Param Assoc");
      Registration.Register_Routine
        (T, Test_Call_Expr_No_Arguments'Access,
         "Call expr with Empty Argument List");
      Registration.Register_Routine
        (T, Test_If_Stmt_Empty_Else'Access,
         "If statement with empty else branch");
      Registration.Register_Routine
        (T, Test_Block_Stmt_Empty_Declarative_Part'Access,
         "Block statement with empty Declarative part");
   end Register_Tests;

end Test_Replacer;
