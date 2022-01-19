with Ada.Containers;              use Ada.Containers;
with AUnit.Assertions;            use AUnit.Assertions;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Missing.AUnit.Assertions;    use Missing.AUnit.Assertions;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
use Rejuvenation.Simple_Factory.Analysis_Units;
with Rejuvenation.Utils; use Rejuvenation.Utils;

with String_Vectors; use String_Vectors;
with Generators;     use Generators;
with Make_Ada;       use Make_Ada;

package body Test_Match_Patterns is

   procedure Assert is new Generic_Assert (Count_Type);

   --  Test Functions

   procedure Assert_Match_Full
     (GR : Grammar_Rule; Pattern_Str : String; Instance_Str : String);
   procedure Assert_Match_Full
     (GR : Grammar_Rule; Pattern_Str : String; Instance_Str : String)
   is
      Pattern  : constant Analysis_Unit := Analyze_Fragment (Pattern_Str, GR);
      Instance : constant Analysis_Unit := Analyze_Fragment (Instance_Str, GR);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Condition => Actual,
         Message   =>
           "Instance doesn't match pattern unexpectedly." & ASCII.CR &
           ASCII.LF & "Instance = " & Instance_Str & ASCII.CR & ASCII.LF &
           "Pattern = " & Pattern_Str & ASCII.CR & ASCII.LF);
   end Assert_Match_Full;

   procedure Assert_Mismatch_Full
     (GR : Grammar_Rule; Pattern_Str : String; Instance_Str : String);
   procedure Assert_Mismatch_Full
     (GR : Grammar_Rule; Pattern_Str : String; Instance_Str : String)
   is
      Pattern  : constant Analysis_Unit := Analyze_Fragment (Pattern_Str, GR);
      Instance : constant Analysis_Unit := Analyze_Fragment (Instance_Str, GR);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Condition => not Actual,
         Message   =>
           "Instance matches pattern unexpectedly." & ASCII.CR & ASCII.LF &
           "Instance = " & Instance_Str & ASCII.CR & ASCII.LF & "Pattern = " &
           Pattern_Str & ASCII.CR & ASCII.LF);
   end Assert_Mismatch_Full;

   procedure Assert_Match_Full (G : Generator);
   procedure Assert_Match_Full (G : Generator) is
      Pattern  : constant Analysis_Unit := Generate_Pattern (G);
      Instance : constant Analysis_Unit := Generate_Instance (G);
   begin
      declare
         MP     : Match_Pattern;
         Actual : constant Boolean :=
           Match_Full (MP, Pattern.Root, Instance.Root);
      begin
         Assert
           (Condition => Actual,
            Message   =>
              "Instance doesn't match pattern." & ASCII.LF & "Instance : " &
              Raw_Signature (Instance.Root) & ASCII.LF & "Pattern  : " &
              Raw_Signature (Pattern.Root));
         declare
            Expected : constant String_Vectors.Vector := Get_Values (G);
            Actual   : constant Node_List.Vector      :=
              MP.Get_Placeholder_As_Nodes (Get_Name (G));
         begin
            Assert
              (Condition => Expected.Length = Actual.Length,
               Message   =>
                 "Number of Actual and Expected Nodes don't match" & ASCII.CR &
                 ASCII.LF & "Actual = " & Actual.Length'Image & ASCII.CR &
                 ASCII.LF & "Expected = " & Expected.Length'Image & ASCII.CR &
                 ASCII.LF);
            for Index in 1 .. Natural (Expected.Length) loop
               Assert
                 (Actual   => Raw_Signature (Actual.Element (Index)),
                  Expected => Expected.Element (Index),
                  Message  => "Value differs at " & Index'Image);
            end loop;
         end;
      end;
   end Assert_Match_Full;

   procedure Test_Ada_Leaves (T : in out Test_Case'Class);
   procedure Test_Ada_Leaves (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  In Ada, identifiers and keywords are case insensitive
      Assert_Match_Full (Identifier_Rule, "true", "true");
      Assert_Match_Full (Identifier_Rule, "true", "True");
      Assert_Match_Full (Identifier_Rule, "true", "TRUE");

      --  In Ada, string literals are case sensitive
      Assert_Match_Full (String_Literal_Rule, """true""", """true""");
      Assert_Mismatch_Full (String_Literal_Rule, """true""", """True""");
      Assert_Mismatch_Full (String_Literal_Rule, """true""", """TRUE""");

      --  In Ada, character literals are case sensitive
      Assert_Match_Full (Char_Literal_Rule, "'t'", "'t'");
      Assert_Mismatch_Full (Char_Literal_Rule, "'t'", "'T'");

      --  In Ada, integers can be written differently
      --  e.g. digits might be grouped
      --       scientific notation might be used
      --       different casing can be used
      --          (both in scientific and hexadecimal notation)
      declare
         Representations_Of_1_Million : constant Analysis_Units.Vector :=
           Analyze_Fragment ("1000000", Int_Literal_Rule) &
           Analyze_Fragment ("1_000_000", Int_Literal_Rule) &
           Analyze_Fragment ("1E6", Int_Literal_Rule) &
           Analyze_Fragment ("1e+6", Int_Literal_Rule) &
           Analyze_Fragment ("16#F4240#", Int_Literal_Rule) &
           Analyze_Fragment ("16#f4240#", Int_Literal_Rule);
      begin
         for Repr1 of Representations_Of_1_Million loop
            for Repr2 of Representations_Of_1_Million loop
               declare
                  MP : Match_Pattern;
               begin
                  Assert
                    (Condition => Match_Full (MP, Repr1.Root, Repr2.Root),
                     Message   =>
                       "Representations mismatch unexpectedly." & ASCII.CR &
                       ASCII.LF & "Repr1 = " & Image (Repr1.Text) & ASCII.CR &
                       ASCII.LF & "Repr2 = " & Image (Repr2.Text));
               end;
            end loop;
         end loop;
      end;

      --  In Ada, reals (written in scientific notation) are case insensitive
      Assert_Match_Full (Num_Literal_Rule, "1.2345E+2", "1.2345E+2");
      Assert_Match_Full (Num_Literal_Rule, "1.2345E+2", "1.2345e+2");
      --  In Ada, reals can be written differently
      --  We don't handle that yet in the rejuvenation library
      --  See https://gt3-prod-2.adacore.com/#/tickets/U922-027
      Assert_Mismatch_Full (Num_Literal_Rule, "1.2345E+2", "123.45");
      Assert_Mismatch_Full (Num_Literal_Rule, "1.2345E+2", "0.12345e3");
   end Test_Ada_Leaves;

   function Make_Basic_Decl (S : String_Vectors.Vector) return String;
   function Make_Basic_Decl (S : String_Vectors.Vector) return String is
   begin
      Assert (S.Length <= 1, "Expected Length <= 1, yet is " & S.Length'Image);
      return
        (if S.Is_Empty then Make_Object_Declaration_Subtype_Indication
         else Make_Object_Declaration_Subtype_Indication
             (Expression => S.First_Element));
   end Make_Basic_Decl;

   procedure Test_Ada_Basic_Decl (T : in out Test_Case'Class);
   procedure Test_Ada_Basic_Decl (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Basic_Decl (Args : String_Vectors.Vector);
      procedure Test_Basic_Decl (Args : String_Vectors.Vector) is
         G : constant Generator :=
           Make_Generator
             ("$M_Expr", Args, Basic_Decl_Rule, Make_Basic_Decl'Access);
      begin
         Assert_Match_Full (G);
      end Test_Basic_Decl;

   begin
      Assert_Match_Full (Basic_Decl_Rule, "$S_Var : $S_Type;", "X : Integer;");
      Assert_Match_Full
        (Basic_Decl_Rule, "$S_Var : $S_Type := $S_Expr;", "X : Integer := 3;");
      Assert_Match_Full
        (Basic_Decl_Rule, "$S_Var : constant $S_Type;",
         "X : constant Integer;");
      Assert_Match_Full
        (Basic_Decl_Rule, "$S_Var : aliased $S_Type;", "X : aliased Integer;");
      Assert_Match_Full
        (Basic_Decl_Rule, "$S_Var : constant $S_Type := $S_Expr;",
         "X : constant Integer := 3;");
      Assert_Match_Full
        (Basic_Decl_Rule, "$S_Var : aliased $S_Type := $S_Expr;",
         "X : aliased Integer := 3;");

      Assert_Mismatch_Full
        (Basic_Decl_Rule, "$S_Var : $S_Type;", "X : Integer := 3;");
      Assert_Mismatch_Full
        (Basic_Decl_Rule, "$S_Var : $S_Type;", "X : constant Integer;");
      Assert_Mismatch_Full
        (Basic_Decl_Rule, "$S_Var : $S_Type;", "X : aliased Integer;");

      Assert_Mismatch_Full
        (Basic_Decl_Rule, "$S_Var : $S_Type := $S_Expr;", "X : Integer;");

      Test_Basic_Decl (String_Vectors.Empty_Vector);
      Test_Basic_Decl (To_Vector ("3", 1));
   end Test_Ada_Basic_Decl;

   function Make_Assignment_Stmt (S : String_Vectors.Vector) return String;
   function Make_Assignment_Stmt (S : String_Vectors.Vector) return String is
   begin
      Assert (S.Length = 1, "Expected Length 1, yet is " & S.Length'Image);
      return Make_Assignment_Statement (Expression => S.First_Element);
   end Make_Assignment_Stmt;

   procedure Test_Assignment_Stmt (T : in out Test_Case'Class);
   procedure Test_Assignment_Stmt (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Assignment_Stmt (Value : String);
      procedure Test_Assignment_Stmt (Value : String) is
         G : constant Generator :=
           Make_Generator
             ("$S_Expr", To_Vector (Value, 1), Stmt_Rule,
              Make_Assignment_Stmt'Access);
      begin
         Assert_Match_Full (G);
      end Test_Assignment_Stmt;

   begin
      Test_Assignment_Stmt ("42");
      Test_Assignment_Stmt ("21 + 21");
      Test_Assignment_Stmt ("Y");
      Test_Assignment_Stmt ("f (3)");
      Test_Assignment_Stmt ("Matrix (4 .. 6)");
   end Test_Assignment_Stmt;

   procedure Test_If_Stmt (T : in out Test_Case'Class);
   procedure Test_If_Stmt (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Pattern : constant String :=
        "if $S_Cond then $M_Stmts_True; else $M_Stmts_False; end if;";
   begin
      Assert_Match_Full (If_Stmt_Rule, Pattern, "if x then null; end if;");
      --  libadalang uses an empty stmt list to represent an absent else branch
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x then null; else null; end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x then f(x+1); else f(x-1); end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x in A | B then null; else null; end if;");
   end Test_If_Stmt;

   procedure Test_Back_Reference (T : in out Test_Case'Class);
   procedure Test_Back_Reference (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Pattern : constant String :=
        "if $S_Cond then $M_Stmts; else $M_Stmts; end if;";
   begin
      Assert_Mismatch_Full (If_Stmt_Rule, Pattern, "if x then null; end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x then null; else null; end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x then P; Q; else P; Q; end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern, "if x then P;    Q; else P; Q; end if;");
      Assert_Match_Full
        (If_Stmt_Rule, Pattern,
         "if x then P; -- comment" & ASCII.LF & " Q; else P; Q; end if;");
      Assert_Mismatch_Full
        (If_Stmt_Rule,
         Pattern,                   --  TODO: Is this what we want?
         "if x then f(x, y); else f ( x ,  y) ; end if;");
      Assert_Mismatch_Full
        (If_Stmt_Rule, Pattern, "if x then P; Q; else P; X; end if;");
   end Test_Back_Reference;

   procedure Test_Call_Mismatch_No_Arguments (T : in out Test_Case'Class);
   procedure Test_Call_Mismatch_No_Arguments (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Mismatch_Full
        (Call_Stmt_Rule,
         Make_Procedure_Call_Statement
           (Actual_Parameter_Part => To_Vector ("$M_Args", 1)),
         Make_Procedure_Call_Statement);
      Assert_Mismatch_Full
        (Expr_Rule,
         Make_Function_Call
           (Actual_Parameter_Part => To_Vector ("$M_Args", 1)),
         Make_Function_Call);
   end Test_Call_Mismatch_No_Arguments;

   function Make_Call_Stmt_From_Name
     (Name : String_Vectors.Vector) return String;
   function Make_Call_Stmt_From_Name
     (Name : String_Vectors.Vector) return String
   is
      Actual_Parameter_Part : constant String_Vectors.Vector := "3" & "4";
   begin
      Assert
        (Actual  => Name.Length, Expected => 1,
         Message => "Precondition Make_Call_Stmt_From_Name violated");
      return
        Make_Procedure_Call_Statement
          (Name.First_Element, Actual_Parameter_Part);
   end Make_Call_Stmt_From_Name;

   procedure Test_Function_Call_Name (T : in out Test_Case'Class);
   procedure Test_Function_Call_Name (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Function_Call_Name
        (Placeholder_Name, Procedure_Name : String);
      procedure Test_Function_Call_Name
        (Placeholder_Name, Procedure_Name : String)
      is
         G : constant Generator :=
           Make_Generator
             (Placeholder_Name, To_Vector (Procedure_Name, 1), Call_Stmt_Rule,
              Make_Call_Stmt_From_Name'Access);
      begin
         Assert_Match_Full (G);
      end Test_Function_Call_Name;

   begin
      Test_Function_Call_Name ("$S_Name", "My_Procedure");
      Test_Function_Call_Name ("$M_Name", "Your_Procedure");
   end Test_Function_Call_Name;

   function Make_Call_Stmt_From_Arguments
     (Args : String_Vectors.Vector) return String is
     (Make_Procedure_Call_Statement (Actual_Parameter_Part => Args));

   procedure Test_Function_Call_Arguments (T : in out Test_Case'Class);
   procedure Test_Function_Call_Arguments (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Function_Call_Arguments (Values : String_Vectors.Vector);
      procedure Test_Function_Call_Arguments (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Args", Values, Call_Stmt_Rule,
              Make_Call_Stmt_From_Arguments'Access);
      begin
         Assert_Match_Full (G);
      end Test_Function_Call_Arguments;

   begin
      Test_Function_Call_Arguments (To_Vector ("1", 1));
      Test_Function_Call_Arguments ("1" & "2");
      Test_Function_Call_Arguments ("g(3)" & "h(1,2)");
   end Test_Function_Call_Arguments;

   function Make_Call_Stmt_From_Arguments_Tail
     (Args : String_Vectors.Vector) return String is
     (Make_Procedure_Call_Statement (Actual_Parameter_Part => "42" & Args));

   procedure Test_Function_Call_Arguments_Tail (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Function_Call_Arguments_Tail
        (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Args", Values, Call_Stmt_Rule,
              Make_Call_Stmt_From_Arguments_Tail'Access);
      begin
         Assert_Match_Full (G);
      end Test_Function_Call_Arguments_Tail;

   begin
      Test_Function_Call_Arguments_Tail (String_Vectors.Empty_Vector);
      Test_Function_Call_Arguments_Tail (String_Vectors.To_Vector ("1", 1));
      Test_Function_Call_Arguments_Tail ("1" & "2");
      Test_Function_Call_Arguments_Tail ("g(3)" & "h(1,2)");
   end Test_Function_Call_Arguments_Tail;

   function Make_Call_Stmt_From_Arguments_Lead
     (Args : String_Vectors.Vector) return String is
     (Make_Procedure_Call_Statement (Actual_Parameter_Part => Args & "42"));

   procedure Test_Function_Call_Arguments_Lead (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Function_Call_Arguments_Lead
        (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Args", Values, Call_Stmt_Rule,
              Make_Call_Stmt_From_Arguments_Lead'Access);
      begin
         Assert_Match_Full (G);
      end Test_Function_Call_Arguments_Lead;

   begin
      Test_Function_Call_Arguments_Lead (String_Vectors.Empty_Vector);
      Test_Function_Call_Arguments_Lead (String_Vectors.To_Vector ("1", 1));
      Test_Function_Call_Arguments_Lead ("1" & "2");
      Test_Function_Call_Arguments_Lead ("g(3)" & "h(1,2)");
   end Test_Function_Call_Arguments_Lead;

   function Make_Object_Decl_From_Type
     (S : String_Vectors.Vector) return String
   is
   begin
      Assert (S.Length = 1, "Expected Length 1, yet is " & S.Length'Image);
      return
        Make_Object_Declaration_Subtype_Indication
          (Subtype_Indication => S.First_Element);
   end Make_Object_Decl_From_Type;

   procedure Test_Object_Decl_Type (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Object_Decl (Value : String) is
         G : constant Generator :=
           Make_Generator
             ("$S_Type", To_Vector (Value, 1), Object_Decl_Rule,
              Make_Object_Decl_From_Type'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Decl;

   begin
      Test_Object_Decl ("Integer");
      Test_Object_Decl ("String");
      Test_Object_Decl ("MyType");
      Test_Object_Decl ("MyPackage.MyType");
      Test_Object_Decl ("Integer (1..10)");
      Test_Object_Decl ("Integer (First..Last)");
      Test_Object_Decl ("Integer (Y'Range)");
      Test_Object_Decl ("Integer (2 .. 5, 3 .. 5)");
   end Test_Object_Decl_Type;

   function Make_Object_Decl_From_Defining_Names
     (Ids : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Defining_Identifier_List => Ids));

   procedure Test_Object_Decl_Defining_Names (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Object_Decl (Values : String_Vectors.Vector) is
         G : constant Generator :=
           Make_Generator
             ("$M_Ids", Values, Object_Decl_Rule,
              Make_Object_Decl_From_Defining_Names'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Decl;

   begin
      Test_Object_Decl (String_Vectors.To_Vector ("x", 1));
      Test_Object_Decl ("v1" & "v2");
      Test_Object_Decl (String_Vectors.To_Vector ("a", 1) & "b" & "c" & "d");
   end Test_Object_Decl_Defining_Names;

   function Make_Object_Decl_From_Defining_Names_Tail
     (Ids : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Defining_Identifier_List => "u" & Ids));

   procedure Test_Object_Decl_Defining_Names_Tail (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Object_Decl (Values : String_Vectors.Vector) is
         G : constant Generator :=
           Make_Generator
             ("$M_Ids", Values, Object_Decl_Rule,
              Make_Object_Decl_From_Defining_Names_Tail'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Decl;

   begin
      Test_Object_Decl (String_Vectors.Empty_Vector);
      Test_Object_Decl (String_Vectors.To_Vector ("x", 1));
      Test_Object_Decl ("v1" & "v2");
      Test_Object_Decl (String_Vectors.To_Vector ("a", 1) & "b" & "c" & "d");
   end Test_Object_Decl_Defining_Names_Tail;

   function Make_Object_Decl_From_Defining_Names_Lead
     (Ids : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Defining_Identifier_List => Ids & "u"));

   procedure Test_Object_Decl_Defining_Names_Lead (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Object_Decl (Values : String_Vectors.Vector) is
         G : constant Generator :=
           Make_Generator
             ("$M_Ids", Values, Object_Decl_Rule,
              Make_Object_Decl_From_Defining_Names_Lead'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Decl;

   begin
      Test_Object_Decl (String_Vectors.Empty_Vector);
      Test_Object_Decl (String_Vectors.To_Vector ("x", 1));
      Test_Object_Decl ("v1" & "v2");
      Test_Object_Decl (String_Vectors.To_Vector ("a", 1) & "b" & "c" & "d");
   end Test_Object_Decl_Defining_Names_Lead;

   function Make_Object_Decl_From_Default_Expression
     (S : String_Vectors.Vector) return String
   is
   begin
      case S.Length is
         when 0 =>
            return Make_Object_Declaration_Subtype_Indication;
         when 1 =>
            return
              Make_Object_Declaration_Subtype_Indication
                (Expression => S.First_Element);
         when others =>
            Assert (False, "Expected Length <= 1, yet is " & S.Length'Image);
            return "ERROR - Should not happen due to Assert False in front";
      end case;
   end Make_Object_Decl_From_Default_Expression;

   procedure Test_Object_Default_Expression (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Object_Default_Expression
        (Pattern : String; Expression : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             (Pattern, Expression, Object_Decl_Rule,
              Make_Object_Decl_From_Default_Expression'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Default_Expression;

   begin
      Test_Object_Default_Expression
        ("$S_Expr", String_Vectors.To_Vector ("2", 1));
      Test_Object_Default_Expression ("$M_Expr", String_Vectors.Empty_Vector);
      Test_Object_Default_Expression
        ("$M_Expr", String_Vectors.To_Vector ("2", 1));
   end Test_Object_Default_Expression;

   function Make_Object_Decl_From_Aspects
     (Aspects : String_Vectors.Vector) return String
   is
   begin
      return
        Make_Object_Declaration_Subtype_Indication (Aspect_List => Aspects);
   end Make_Object_Decl_From_Aspects;

   procedure Test_Object_Aspects (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Object_Aspects
        (Pattern : String; Aspects : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             (Pattern, Aspects, Object_Decl_Rule,
              Make_Object_Decl_From_Aspects'Access);
      begin
         Assert_Match_Full (G);
      end Test_Object_Aspects;

   begin
      Test_Object_Aspects
        ("$S_Aspect", String_Vectors.To_Vector ("Atomic", 1));
      Test_Object_Aspects
        ("$S_Aspect", String_Vectors.To_Vector ("Unreferenced => True", 1));
      Test_Object_Aspects
        ("$M_Aspects", String_Vectors.To_Vector ("Address => 12", 1));
      Test_Object_Aspects ("$M_Aspects", "Alignment => 4" & "Size => 64");
      Test_Object_Aspects ("$M_Aspects", String_Vectors.Empty_Vector);
   end Test_Object_Aspects;

   function Make_Subtype_Indication_Decl_Ranges
     (Ranges : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Subtype_Indication =>
           Make_Subtype_Indication_Index_Constraints
             (Index_Constraints => Ranges)));

   procedure Test_Subtype_Indication_Decl_Ranges (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Subtype_Indication_Decl_Ranges
        (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Ranges", Values, Object_Decl_Rule,
              Make_Subtype_Indication_Decl_Ranges'Access);
      begin
         Assert_Match_Full (G);
      end Test_Subtype_Indication_Decl_Ranges;

   begin
      Test_Subtype_Indication_Decl_Ranges
        (String_Vectors.To_Vector ("2 .. 5", 1));
      Test_Subtype_Indication_Decl_Ranges
        (String_Vectors.To_Vector ("Y'Range", 1));
      Test_Subtype_Indication_Decl_Ranges ("4 .. 6" & "M .. N");
   end Test_Subtype_Indication_Decl_Ranges;

   function Make_Subtype_Indication_Decl_Ranges_Tail
     (Ranges : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Subtype_Indication =>
           Make_Subtype_Indication_Index_Constraints

             (Index_Constraints => "1 .. 10" & Ranges)));

   procedure Test_Subtype_Indication_Decl_Ranges_Tail
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Subtype_Indication_Decl_Ranges_Tail
        (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Ranges", Values, Object_Decl_Rule,
              Make_Subtype_Indication_Decl_Ranges_Tail'Access);
      begin
         Assert_Match_Full (G);
      end Test_Subtype_Indication_Decl_Ranges_Tail;

   begin
      Test_Subtype_Indication_Decl_Ranges_Tail (String_Vectors.Empty_Vector);
      Test_Subtype_Indication_Decl_Ranges_Tail
        (String_Vectors.To_Vector ("2 .. 5", 1));
      Test_Subtype_Indication_Decl_Ranges_Tail
        (String_Vectors.To_Vector ("Y'Range", 1));
      Test_Subtype_Indication_Decl_Ranges_Tail ("4 .. 6" & "M .. N");
   end Test_Subtype_Indication_Decl_Ranges_Tail;

   function Make_Subtype_Indication_Decl_Ranges_Lead
     (Ranges : String_Vectors.Vector) return String is
     (Make_Object_Declaration_Subtype_Indication
        (Subtype_Indication =>
           Make_Subtype_Indication_Index_Constraints
             (Index_Constraints => Ranges & "7..12")));

   procedure Test_Subtype_Indication_Decl_Ranges_Lead
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Subtype_Indication_Decl_Ranges_Lead
        (Values : String_Vectors.Vector)
      is
         G : constant Generator :=
           Make_Generator
             ("$M_Ranges", Values, Object_Decl_Rule,
              Make_Subtype_Indication_Decl_Ranges_Lead'Access);
      begin
         Assert_Match_Full (G);
      end Test_Subtype_Indication_Decl_Ranges_Lead;

   begin
      Test_Subtype_Indication_Decl_Ranges_Lead (String_Vectors.Empty_Vector);
      Test_Subtype_Indication_Decl_Ranges_Lead
        (String_Vectors.To_Vector ("2 .. 5", 1));
      Test_Subtype_Indication_Decl_Ranges_Lead
        (String_Vectors.To_Vector ("Y'Range", 1));
      Test_Subtype_Indication_Decl_Ranges_Lead ("4 .. 6" & "M .. N");
   end Test_Subtype_Indication_Decl_Ranges_Lead;

   procedure Test_Subtype_Indication_Decl_Both (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Type_Key    : constant String := "$S_Type";
      Ranges_Key  : constant String := "$M_Ranges";
      Pattern_Str : constant String :=
        Make_Object_Declaration_Subtype_Indication
          (Subtype_Indication =>
             Make_Subtype_Indication_Index_Constraints
               (Subtype_Mark      => Type_Key,
                Index_Constraints => To_Vector (Ranges_Key, 1)));
      Pattern : constant Analysis_Unit :=
        Analyze_Fragment (Pattern_Str, Object_Decl_Rule);

      Expected_Type_Value   : constant String                := "String";
      Expected_Ranges_Value : constant String_Vectors.Vector :=
        "1..10" & "2..12";
      Instance_Str : constant String :=
        Make_Object_Declaration_Subtype_Indication
          (Subtype_Indication =>
             Make_Subtype_Indication_Index_Constraints
               (Subtype_Mark      => Expected_Type_Value,
                Index_Constraints => Expected_Ranges_Value));
      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Instance_Str, Object_Decl_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Condition => Actual,
         Message   =>
           "Match expected between" & ASCII.LF & "Pattern  : " & Pattern_Str &
           ASCII.LF & "Instance : " & Instance_Str);
      declare
         Actual_Type_Value : constant String :=
           MP.Get_Single_As_Raw_Signature (Type_Key);
         Actual_Ranges_Nodes : constant Node_List.Vector :=
           MP.Get_Multiple_As_Nodes (Ranges_Key);
      begin
         Assert
           (Actual  => Actual_Type_Value, Expected => Expected_Type_Value,
            Message => "Types differ");
         declare
            Offset : constant Integer :=
              Expected_Ranges_Value.First_Index -
              Actual_Ranges_Nodes.First_Index;
         begin
            for Index in
              Expected_Ranges_Value.First_Index ..
                Expected_Ranges_Value.Last_Index
            loop
               Assert
                 (Actual =>
                    Raw_Signature
                      (Actual_Ranges_Nodes.Element (Index - Offset)),
                  Expected => Expected_Ranges_Value.Element (Index),
                  Message  => "Ranges differ at " & Index'Image);
            end loop;
         end;
      end;
   end Test_Subtype_Indication_Decl_Both;

   procedure Test_Subtype_Indication_Decl_Mismatch_Object_Decl
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert_Mismatch_Full
        (Object_Decl_Rule, "X : Integer ($M_Ranges);", "X : Integer;");
   end Test_Subtype_Indication_Decl_Mismatch_Object_Decl;

   procedure Test_Stmt (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String        := "$S_Stmt";
      Pattern_Str      : constant String        := Placeholder_Name & ";";
      Pattern          : constant Analysis_Unit :=
        Analyze_Fragment (Pattern_Str, Stmt_Rule);

      Instance_Str : constant String        := "null;";
      Instance     : constant Analysis_Unit :=
        Analyze_Fragment (Instance_Str, Stmt_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Condition => Actual, Message => "Instance doesn't match pattern.");
      Assert
        (Actual   => MP.Get_Placeholder_As_Raw_Signature (Placeholder_Name),
         Expected => Instance_Str, Message => "Placeholder value differ");
   end Test_Stmt;

   procedure Test_Label (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String        := "$S_Stmt";
      Pattern_Str      : constant String        := Placeholder_Name & ";";
      Pattern          : constant Analysis_Unit :=
        Analyze_Fragment (Pattern_Str, Stmt_Rule);

      Rules : constant array (1 .. 2) of Grammar_Rule :=
        (Label_Rule, Stmts_Rule);
   begin
      for Rule of Rules loop
         declare
            Instance_Str : constant String        := "<<label>>";
            Instance     : constant Analysis_Unit :=
              Analyze_Fragment (Instance_Str, Rule);

            MP     : Match_Pattern;
            Actual : constant Boolean :=
              Match_Full (MP, Pattern.Root, Instance.Root);
         begin
            Assert
              (Condition => Actual,
               Message => "Instance doesn't match pattern with " & Rule'Image);
            Assert
              (Actual =>
                 MP.Get_Placeholder_As_Raw_Signature (Placeholder_Name),
               Expected => Instance_Str,
               Message  => "Placeholder value differ with " & Rule'Image);
         end;
      end loop;
   end Test_Label;

   procedure Test_Pragma (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String        := "$S_Stmt";
      Pattern_Str      : constant String        := Placeholder_Name & ";";
      Pattern          : constant Analysis_Unit :=
        Analyze_Fragment (Pattern_Str, Stmt_Rule);

      Rules : constant array (1 .. 3) of Grammar_Rule :=
        (Pragma_Rule, Stmt_Rule, Stmts_Rule);
   begin
      for Rule of Rules loop
         declare
            Instance_Str : constant String := "pragma Unreferenced (T);";
            Instance     : constant Analysis_Unit :=
              Analyze_Fragment (Instance_Str, Rule);

            MP     : Match_Pattern;
            Actual : constant Boolean :=
              Match_Full (MP, Pattern.Root, Instance.Root);
         begin
            Assert
              (Condition => Actual,
               Message   => "Instance doesn't match pattern.");
            Assert
              (Actual =>
                 MP.Get_Placeholder_As_Raw_Signature (Placeholder_Name),
               Expected => Instance_Str,
               Message  => "Placeholder value differ");
         end;
      end loop;
   end Test_Pragma;

   procedure Test_Stmts (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Placeholder_Name : constant String        := "$M_Stmts";
      Pattern_Str      : constant String        := Placeholder_Name & ";";
      Pattern          : constant Analysis_Unit :=
        Analyze_Fragment
          (Pattern_Str, Stmt_Rule);   --  Note: Stmts_Rule is NOT needed!

      Instance_Str : constant String        := "null; null; null;";
      Instance     : constant Analysis_Unit :=
        Analyze_Fragment (Instance_Str, Stmts_Rule);

      MP     : Match_Pattern;
      Actual : constant Boolean :=
        Match_Full (MP, Pattern.Root, Instance.Root);
   begin
      Assert
        (Condition => Actual, Message => "Instance doesn't match pattern.");
      Assert
        (Actual   => MP.Get_Placeholder_As_Raw_Signature (Placeholder_Name),
         Expected => Instance_Str, Message => "Placeholder value differ");
      goto label;
      <<label>>
   end Test_Stmts;

   --  Test plumbing

   overriding function Name
     (T : Match_Patterns_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Match Pattern");
   end Name;

   overriding procedure Register_Tests (T : in out Match_Patterns_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Ada_Leaves'Access, "Ada Leaves: Identifier & literals");
      Registration.Register_Routine
        (T, Test_Ada_Basic_Decl'Access, "Basic Declaration");
      Registration.Register_Routine
        (T, Test_Assignment_Stmt'Access, "Assignment Statement");
      Registration.Register_Routine (T, Test_If_Stmt'Access, "If Statement");
      Registration.Register_Routine
        (T, Test_Call_Mismatch_No_Arguments'Access,
         "Pattern for Subprogram Call with variable number of arguments " &
         "mismatches Subprogram call with No Arguments");
      Registration.Register_Routine
        (T, Test_Function_Call_Name'Access, "Function Call Name");
      Registration.Register_Routine
        (T, Test_Function_Call_Arguments'Access,
         "Function Call with Arguments");
      Registration.Register_Routine
        (T, Test_Function_Call_Arguments_Tail'Access,
         "Function Call with Arguments Tail");
      Registration.Register_Routine
        (T, Test_Function_Call_Arguments_Lead'Access,
         "Function Call with Arguments Lead");
      Registration.Register_Routine
        (T, Test_Object_Decl_Type'Access, "Object Declaration - Type");
      Registration.Register_Routine
        (T, Test_Object_Decl_Defining_Names'Access,
         "Object Declaration - Defining Names");
      Registration.Register_Routine
        (T, Test_Object_Decl_Defining_Names_Tail'Access,
         "Object Declaration - Defining Names Tail");
      Registration.Register_Routine
        (T, Test_Object_Decl_Defining_Names_Lead'Access,
         "Object Declaration - Defining Names Lead");
      Registration.Register_Routine
        (T, Test_Object_Default_Expression'Access,
         "Object Declaration - Default Expression");
      Registration.Register_Routine
        (T, Test_Object_Aspects'Access, "Object Declaration - Aspects");
      Registration.Register_Routine
        (T, Test_Subtype_Indication_Decl_Ranges'Access,
         "Subtype Indication Declaration with Ranges");
      Registration.Register_Routine
        (T, Test_Subtype_Indication_Decl_Ranges_Tail'Access,
         "Subtype Indication Declaration with Ranges Tail");
      Registration.Register_Routine
        (T, Test_Subtype_Indication_Decl_Ranges_Lead'Access,
         "Subtype Indication Declaration with Ranges Lead");
      Registration.Register_Routine
        (T, Test_Subtype_Indication_Decl_Both'Access,
         "Subtype Indication Declaration with both Type and Ranges");
      Registration.Register_Routine
        (T, Test_Subtype_Indication_Decl_Mismatch_Object_Decl'Access,
         "Subtype_Indication Declaration mismatch with Object Declaration");
      Registration.Register_Routine
        (T, Test_Back_Reference'Access, "Backreferences in Find");
      Registration.Register_Routine
        (T, Test_Stmt'Access, "Value of Stmt placeholder");
      Registration.Register_Routine (T, Test_Label'Access, "Match with label");
      Registration.Register_Routine
        (T, Test_Pragma'Access, "Match with pragma");
      Registration.Register_Routine
        (T, Test_Stmts'Access, "Value of Stmts placeholder");
   end Register_Tests;

end Test_Match_Patterns;
