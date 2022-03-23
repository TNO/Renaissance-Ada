with Ada.Strings.Fixed;              use Ada.Strings.Fixed;
with AUnit.Assertions;               use AUnit.Assertions;
with Libadalang.Analysis;            use Libadalang.Analysis;
with Libadalang.Common;              use Libadalang.Common;
with Rejuvenation;                   use Rejuvenation;
with Rejuvenation.Find_And_Replacer; use Rejuvenation.Find_And_Replacer;
with Rejuvenation.Patterns;          use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory;    use Rejuvenation.Simple_Factory;
with Rejuvenation.Text_Rewrites;     use Rejuvenation.Text_Rewrites;
with Assert_AST;                     use Assert_AST;

package body Test_Find_And_Replacer is

   --  Test Functions
   procedure Test_Basic_Find_And_Replacer (T : in out Test_Case'Class);
   procedure Test_Basic_Find_And_Replacer (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Find_Pattern : constant Pattern := Make_Pattern ("A;", Call_Stmt_Rule);
      --  I would like to have it also working with "A;A;" with Stmts_Rule
      Replace_Pattern : constant Pattern :=
        Make_Pattern ("B;", Call_Stmt_Rule);

      function Make_Composition (S : String) return String;
      function Make_Composition (S : String) return String is
         Irrelevant_Prefix  : constant String := "C;";
         Irrelevant_Postfix : constant String := "D;";
      begin
         return Irrelevant_Prefix & S & Irrelevant_Postfix;
      end Make_Composition;

   begin
      for Index in 0 .. 5 loop
         declare
            Unit : constant Analysis_Unit :=
              Analyze_Fragment
                (Make_Composition (Index * Find_Pattern.Get_String),
                 Stmts_Rule);
            Expected : constant String :=
              Make_Composition (Index * Replace_Pattern.Get_String);
            TR : Text_Rewrite'Class := Make_Text_Rewrite_Unit (Unit);
         begin
            Find_And_Replace (TR, Unit.Root, Find_Pattern, Replace_Pattern);
            Assert
              (Actual  => TR.ApplyToString, Expected => Expected,
               Message =>
                 "Find and Replace with Basic case failed - Index = " &
                 Index'Image);
         end;
      end loop;
   end Test_Basic_Find_And_Replacer;

   procedure Test_Case_Stmt_Find_And_Replacer (T : in out Test_Case'Class);
   procedure Test_Case_Stmt_Find_And_Replacer (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Find_Pattern : constant Pattern :=
        Make_Pattern
          ("case $S_Expr is " & "when $M_Values => $M_Stmts_True;" &
           "when others => $M_Stmts_False;" & "end case;",
           Case_Stmt_Rule);
      Replace_Pattern : constant Pattern :=
        Make_Pattern
          ("if ($S_Expr) in $M_Values " & "then $M_Stmts_True; " &
           "else $M_Stmts_False; " & "end if;",
           If_Stmt_Rule);
      Unit : constant Analysis_Unit :=
        Analyze_Fragment
          ("case c is " & "when '*' | '?' | '+' => in_call;" &
           "when others => out_call;" & "end case;",
           Case_Stmt_Rule);
      Expected : constant String :=
        "if (c) in '*' | '?' | '+' then in_call; else out_call; end if;";
      TR : Text_Rewrite'Class := Make_Text_Rewrite_Unit (Unit);
   begin
      Find_And_Replace (TR, Unit.Root, Find_Pattern, Replace_Pattern);
      Assert_Equal_AST
        (Expected, TR.ApplyToString, If_Stmt_Rule,
         "Find and Replace with Case Stmt failed");
   end Test_Case_Stmt_Find_And_Replacer;

   procedure Test_If_Stmt_Find_And_Replacer (T : in out Test_Case'Class);
   procedure Test_If_Stmt_Find_And_Replacer (T : in out Test_Case'Class) is
      --  libadalang uses an empty stmt list to represent an absent else branch
      --  Replace must handle an empty stmt list at every possible location,
      --  such as stmt list of alternative in case statement
      --  and     stmt list in declare block

      pragma Unreferenced (T);

      Find_Pattern : constant Pattern :=
        Make_Pattern
          ("if $S_Cond " & "then $M_Stmts_True;" & "else $M_Stmts_False;" &
           "end if;",
           If_Stmt_Rule);
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("if c then null; end if;", If_Stmt_Rule);

      Replace_Pattern_If   : Pattern renames Find_Pattern;
      Replace_Pattern_Case : constant Pattern :=
        Make_Pattern
          ("case $S_Cond is " & "when True => $M_Stmts_True;" &
           "when others => $M_Stmts_False;" & "end case;",
           Case_Stmt_Rule);
      Replace_Pattern_Declare : constant Pattern :=
        Make_Pattern
          ("declare X : constant Boolean := $S_Cond; " &
           "begin $M_Stmts_False; end;",
           Block_Stmt_Rule);

      Replace_Patterns : constant array (1 .. 3) of Pattern :=
        (Replace_Pattern_If, Replace_Pattern_Case, Replace_Pattern_Declare);
      TR : Text_Rewrite'Class := Make_Text_Rewrite_Unit (Unit);
   begin
      for Replace_Pattern of Replace_Patterns loop
         Find_And_Replace (TR, Unit.Root, Find_Pattern, Replace_Pattern);
         declare
            Result_Str  : constant String        := TR.ApplyToString;
            Result_Unit : constant Analysis_Unit :=
              Analyze_Fragment (Result_Str, Replace_Pattern.Get_Rule);
         begin
            pragma Unreferenced (Result_Unit);
         end;
      end loop;
   end Test_If_Stmt_Find_And_Replacer;

   procedure Test_Nested_Find_And_Replacer (T : in out Test_Case'Class);
   procedure Test_Nested_Find_And_Replacer (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Find_Pattern : constant Pattern :=
        Make_Pattern
          ("if not $S_Cond then " & "$M_True_Stmts;" &
           "else $S_False_Stmt; $M_False_Stmts;" & "end if;",
           If_Stmt_Rule);
      Replace_Pattern : constant Pattern :=
        Make_Pattern
          ("if $S_Cond then $S_False_Stmt; $M_False_Stmts; " &
           "else $M_True_Stmts; end if;",
           If_Stmt_Rule);
      Unit : constant Analysis_Unit :=
        Analyze_Fragment
          ("if not B then " &
           "if not C then Call_NB_NC; else Call_NB_C; end if;" &
           "else Call_B;" & "end if;",
           If_Stmt_Rule);
      Expected_String : constant String :=
        "if B then Call_B; " &
        "else if C then Call_NB_C; else Call_NB_NC; end if; " & "end if;";
      TR : Text_Rewrite'Class := Make_Text_Rewrite_Unit (Unit);
   begin
      Find_And_Replace (TR, Unit.Root, Find_Pattern, Replace_Pattern);
      Assert_Equal_AST
        (Expected_String, TR.ApplyToString, If_Stmt_Rule,
         "Find and Replace with Nested case failed.");
   end Test_Nested_Find_And_Replacer;

   --  Test plumbing

   overriding function Name
     (T : Find_And_Replacer_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Find_And_Replacer");
   end Name;

   overriding procedure Register_Tests (T : in out Find_And_Replacer_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Basic_Find_And_Replacer'Access, "Find_And_Replacer Basic");
      Registration.Register_Routine
        (T, Test_Case_Stmt_Find_And_Replacer'Access,
         "Find_And_Replacer Case stmt");
      Registration.Register_Routine
        (T, Test_If_Stmt_Find_And_Replacer'Access,
         "Find_And_Replacer If stmt (empty else branch)");

      Registration.Register_Routine
        (T, Test_Nested_Find_And_Replacer'Access, "Find_And_Replacer Nested");
   end Register_Tests;

end Test_Find_And_Replacer;
