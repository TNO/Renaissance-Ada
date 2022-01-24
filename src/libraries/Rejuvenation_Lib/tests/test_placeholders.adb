with Ada.Containers;              use Ada.Containers;
with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Missing.AUnit.Assertions;    use Missing.AUnit.Assertions;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Placeholders;   use Rejuvenation.Placeholders;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with String_Vectors;              use String_Vectors;
with String_Sets;                 use String_Sets;
with String_Sets_Utils;           use String_Sets_Utils;

with Make_Ada; use Make_Ada;

package body Test_Placeholders is

   procedure Assert is new Generic_Assert (Natural);
   procedure Assert is new Generic_Assert (Count_Type);
   procedure Assert is new Generic_Assert (Ada_Node_Kind_Type);

   --  Test Functions

   procedure Test_Defining_Name_Placeholder (T : in out Test_Case'Class);
   procedure Test_Defining_Name_Placeholder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String := "$M____MyName12";
      --  TODO make a separate test case to document that
      --  two consecutive underlines not permitted
      --  in normal Ada variables,
      --  but allowed in placeholder names!

      Fragment : constant String :=
        Make_Object_Declaration_Subtype_Indication
          (Defining_Identifier_List => To_Vector (Placeholder_Name, 1));
      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Fragment, Object_Decl_Rule);
   begin
      Assert
        (Condition => Is_Placeholder_Name (Placeholder_Name),
         Message   =>
           "Precondition violated: "
         & "Name is unexpectedly not a placeholder name");
      Assert
        (Actual  => Unit.Root.Kind, Expected => Ada_Object_Decl,
         Message => "Unexpected kind of 'Unit.Root'");
      declare
         O_D : constant Object_Decl        := Unit.Root.As_Object_Decl;
         Ids : constant Defining_Name_List := O_D.F_Ids;
      begin
         Assert
           (Actual  => Ids.Children_Count, Expected => 1,
            Message => "Unexpected count of children of 'Ids'");
         Assert
           (Condition => not Is_Placeholder (Ids),
            Message   => "Unexpected 'Ids' is placeholder");
         Assert
           (Condition => Is_Placeholder (Ids.First_Child),
            Message   => "Unexpected not a placeholder");
      end;
   end Test_Defining_Name_Placeholder;

   procedure Test_Stmt_Placeholder (T : in out Test_Case'Class);
   procedure Test_Stmt_Placeholder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String := "$M_Aap_Noot_Mies";

      Fragment : constant String :=
        Make_Procedure_Call_Statement (Procedure_Name => Placeholder_Name);
      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Fragment, Call_Stmt_Rule);
   begin
      Assert
        (Condition => Is_Placeholder_Name (Placeholder_Name),
         Message   =>
           "Precondition violated: "
         & "Name is unexpectedly not a placeholder name");
      Assert
        (Actual  => Unit.Root.Kind, Expected => Ada_Call_Stmt,
         Message => "Unexpected kind of 'Unit.Root'");
      Assert
        (Condition => Is_Placeholder (Unit.Root),
         Message   => "Unexpected not a placeholder");
   end Test_Stmt_Placeholder;

   procedure Test_Subprogram_Identifier_Placeholder
     (T : in out Test_Case'Class);
   procedure Test_Subprogram_Identifier_Placeholder
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Placeholder_Name : constant String := "$M_379";

      Fragment : constant String :=
        Make_Procedure_Call_Statement
          (Procedure_Name        => Placeholder_Name,
           Actual_Parameter_Part => To_Vector ("3", 1) & "True" & "max");
      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Fragment, Call_Stmt_Rule);
   begin
      Assert
        (Condition => Is_Placeholder_Name (Placeholder_Name),
         Message   =>
           "Precondition violated: "
         & "Name is unexpectedly not a placeholder name");
      Assert
        (Actual  => Unit.Root.Kind, Expected => Ada_Call_Stmt,
         Message => "Unexpected kind of 'Unit.Root'");
      declare
         C_S  : constant Call_Stmt                := Unit.Root.As_Call_Stmt;
         Call : constant Libadalang.Analysis.Name := C_S.F_Call;
      begin
         Assert
           (Actual  => Call.Kind, Expected => Ada_Call_Expr,
            Message => "Unexpected kind of Call");
         Assert
           (Condition => not Is_Placeholder (Call),
            Message   => "Unexpected 'Call' is placeholder");
         declare
            N : constant Libadalang.Analysis.Name := Call.As_Call_Expr.F_Name;
         begin
            Assert
              (Condition => Is_Placeholder (N),
               Message   => "Unexpected not a placeholder");
         end;
      end;
   end Test_Subprogram_Identifier_Placeholder;

   procedure Test_Enum_Literal_Decl_Placeholder (T : in out Test_Case'Class);
   procedure Test_Enum_Literal_Decl_Placeholder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String := "$S__";

      Fragment : constant String :=
        Make_Enumeration_Type_Definition (To_Vector (Placeholder_Name, 1));
      Unit : constant Analysis_Unit :=
        Analyze_Fragment (Fragment, Enum_Type_Def_Rule);
   begin
      Assert
        (Condition => Is_Placeholder_Name (Placeholder_Name),
         Message   =>
           "Precondition violated: "
         & "Name is unexpectedly not a placeholder name");
      Assert
        (Actual  => Unit.Root.Kind, Expected => Ada_Enum_Type_Def,
         Message => "Unexpected kind of 'Unit.Root'");
      declare
         E_T_D   : constant Enum_Type_Def := Unit.Root.As_Enum_Type_Def;
         E_L_D_L : constant Enum_Literal_Decl_List := E_T_D.F_Enum_Literals;
      begin
         Assert
           (Condition => not Is_Placeholder (E_T_D),
            Message   => "Unexpected 'E_T_D' is placeholder");
         Assert
           (Condition => not Is_Placeholder (E_L_D_L),
            Message   => "Unexpected 'E_L_D_L' is placeholder");
         Assert
           (Actual  => E_L_D_L.Children_Count, Expected => 1,
            Message => "Unexpected count of children of 'E_L_D_L'");
         Assert
           (Condition => Is_Placeholder (E_L_D_L.First_Child),
            Message   => "Unexpected not a placeholder");
      end;
   end Test_Enum_Literal_Decl_Placeholder;

   procedure Test_Param_Assoc_Placeholder (T : in out Test_Case'Class);
   procedure Test_Param_Assoc_Placeholder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String := "$M_S";

      Fragment : constant String :=
        Make_Function_Call
          (Actual_Parameter_Part => To_Vector (Placeholder_Name, 1));
      Unit : constant Analysis_Unit := Analyze_Fragment (Fragment, Expr_Rule);
   begin
      Assert
        (Condition => Is_Placeholder_Name (Placeholder_Name),
         Message   =>
           "Precondition violated: "
         & "Name is unexpectedly not a placeholder name");
      Assert
        (Actual  => Unit.Root.Kind, Expected => Ada_Call_Expr,
         Message => "Unexpected kind of 'Unit.Root'");
      declare
         C_E : constant Call_Expr := Unit.Root.As_Call_Expr;
         S   : constant Ada_Node  := C_E.F_Suffix;
      begin
         Assert
           (Condition => not Is_Placeholder (S),
            Message   => "Unexpected 'S' is placeholder");
         Assert
           (Actual  => S.Kind, Expected => Ada_Assoc_List,
            Message => "Unexpected kind of 'S'");
         declare
            A_L : constant Assoc_List := S.As_Assoc_List;
         begin
            Assert
              (Actual  => A_L.Children_Count, Expected => 1,
               Message => "Unexpected count of children of 'A_L'");
            Assert
              (Condition => Is_Placeholder (A_L.First_Child),
               Message   => "Unexpected not a placeholder");
         end;
      end;
   end Test_Param_Assoc_Placeholder;

   procedure Assert_Nodes_In_Order
     (Nodes : Node_List.Vector; Message : String);
   procedure Assert_Nodes_In_Order (Nodes : Node_List.Vector; Message : String)
   is
      Last_Position : Natural := 0;
   begin
      for Node of Nodes loop
         declare
            Start_Position : constant Positive := Start_Offset (Node);
         begin
            Assert
              (Condition => Last_Position < Start_Position,
               Message   =>
                 Message & ASCII.LF & "Nodes not in order" & ASCII.LF &
                 "Last_Position = " & Last_Position'Image & ASCII.LF &
                 "Start_Position = " & Start_Position'Image);
            Last_Position := End_Offset (Node);
         end;
      end loop;
   end Assert_Nodes_In_Order;

   procedure Test_Placeholder_Nodes (T : in out Test_Case'Class);
   procedure Test_Placeholder_Nodes (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Placeholder_Nodes
        (Fragment          : String; Rule : Grammar_Rule;
         NrOf_Placeholders : Count_Type);
      procedure Test_Placeholder_Nodes
        (Fragment          : String; Rule : Grammar_Rule;
         NrOf_Placeholders : Count_Type)
      is
         Unit : constant Analysis_Unit    := Analyze_Fragment (Fragment, Rule);
         Placeholders : constant Node_List.Vector :=
           Get_Placeholders (Unit.Root);
      begin
         Assert
           (Actual  => Placeholders.Length, Expected => NrOf_Placeholders,
            Message => "Number of placeholder nodes differ");

         Assert_Nodes_In_Order
           (Placeholders, "Check placeholders in order failed.");
      end Test_Placeholder_Nodes;

   begin
      Test_Placeholder_Nodes ("$S_Bla", Name_Rule, 1);
      Test_Placeholder_Nodes
        ("if $S_Cond then $M_True_Stmts; else $M_False_Stmts; end if;",
         If_Stmt_Rule, 3);
      Test_Placeholder_Nodes
        ("if $S_Cond then $M_Stmts; else $M_Stmts; end if;", If_Stmt_Rule, 3);
      Test_Placeholder_Nodes
        ("my_Str : constant String := ""$S_Cond"";", Object_Decl_Rule, 0);
   end Test_Placeholder_Nodes;

   procedure Test_Placeholder_Names (T : in out Test_Case'Class);
   procedure Test_Placeholder_Names (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Placeholder_Names
        (Fragment : String; Rule : Grammar_Rule; Expected : Set);
      procedure Test_Placeholder_Names
        (Fragment : String; Rule : Grammar_Rule; Expected : Set)
      is
         Pattern : constant Analysis_Unit := Analyze_Fragment (Fragment, Rule);
         Actual  : constant String_Sets.Set :=
           Get_Placeholder_Names (Pattern.Root);
      begin
         Assert
           (Condition => Actual = Expected,
            Message   =>
              "Placeholder names differ" & ASCII.LF & "Actual = " &
              To_String (Actual) & ASCII.LF & "Expected = " &
              To_String (Expected));
      end Test_Placeholder_Names;

      Placeholder_A : constant String := "$S_Bla";
      Placeholder_C : constant String := "$S_Cond";
      Placeholder_T : constant String := "$M_True_Stmts";
      Placeholder_F : constant String := "$M_False_Stmts";
      Placeholder_S : constant String := "$M_Stmts";

   begin
      Test_Placeholder_Names
        ("$S_Bla", Name_Rule, String_Sets.To_Set (Placeholder_A));
      Test_Placeholder_Names
        ("if " & Placeholder_C & " then " & Placeholder_T & "; else " &
         Placeholder_F & "; end if;",
         If_Stmt_Rule,
         From_Vector
           (String_Vectors.To_Vector (Placeholder_C, 1) & Placeholder_T &
            Placeholder_F));
      Test_Placeholder_Names
        ("if " & Placeholder_C & " then " & Placeholder_S & "; else " &
         Placeholder_S & "; end if;",
         If_Stmt_Rule, From_Vector (Placeholder_C & Placeholder_S));
      Test_Placeholder_Names
        ("my_Str : constant String := ""$S_Cond"";", Object_Decl_Rule,
         String_Sets.Empty_Set);
   end Test_Placeholder_Names;

   --  Test plumbing

   overriding function Name
     (T : Placeholders_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Patterns Placeholders");
   end Name;

   overriding procedure Register_Tests (T : in out Placeholders_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Defining_Name_Placeholder'Access,
         "Defining Name Placeholder");
      Registration.Register_Routine
        (T, Test_Stmt_Placeholder'Access, "Stmt Placeholder");
      Registration.Register_Routine
        (T, Test_Subprogram_Identifier_Placeholder'Access,
         "Subprogram Identifier Placeholder");
      Registration.Register_Routine
        (T, Test_Enum_Literal_Decl_Placeholder'Access,
         "Enum Literal Declaration Placeholder");
      Registration.Register_Routine
        (T, Test_Param_Assoc_Placeholder'Access, "Param Assoc Placeholder");
      --  TODO  Designator Identifier (e.g. $S_X => 12) and
   --       Value Identifier (e.g. param_name => $S_Value) are not Param Assocs
      Registration.Register_Routine
        (T, Test_Placeholder_Nodes'Access, "Placeholder nodes");
      Registration.Register_Routine
        (T, Test_Placeholder_Names'Access, "Placeholder names");
   end Register_Tests;

end Test_Placeholders;
