with AUnit.Assertions;              use AUnit.Assertions;
with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Rejuvenation;                  use Rejuvenation;
with Rejuvenation.Finder;           use Rejuvenation.Finder;
with Rejuvenation.Simple_Factory;   use Rejuvenation.Simple_Factory;
with Predefined_Rewriters_Minimal_Parentheses;
use Predefined_Rewriters_Minimal_Parentheses;

package body Test_Minimal_Parenthesis is

   procedure Assert_Unchanged
     (Input : String; Rule : Grammar_Rule; Message : String);
   procedure Assert_Unchanged
     (Input : String; Rule : Grammar_Rule; Message : String)
   is
      Unit : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      P_Es : constant Node_List.Vector := Find (Unit.Root, Ada_Paren_Expr);
   begin
      Assert ((for all P_E of P_Es =>
                 Are_Parentheses_Necessary (P_E.As_Paren_Expr)),
                 Message);
   end Assert_Unchanged;

   procedure Assert_Changed
     (Input : String; Rule : Grammar_Rule; Message : String);
   procedure Assert_Changed
     (Input : String; Rule : Grammar_Rule; Message : String)
   is
      Unit : constant Analysis_Unit := Analyze_Fragment (Input, Rule);
      P_Es : constant Node_List.Vector := Find (Unit.Root, Ada_Paren_Expr);
   begin
      Assert ((for some P_E of P_Es =>
                  not Are_Parentheses_Necessary (P_E.As_Paren_Expr)),
                 Message);
   end Assert_Changed;

   --  Test Functions

   procedure Test_Mandatory_Parenthesis (T : in out Test_Case'Class);
   procedure Test_Mandatory_Parenthesis (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      Assert_Unchanged
        ("function f return Integer is (2);", Expr_Fn_Rule,
         "Parenthesis are mandatory for Expression Functions");
      Assert_Unchanged
        ("Code'(Dec)", Expr_Rule,
         "Parenthesis are mandatory for Qualified Expressions");
      Assert_Unchanged ("2 * (2/3)", Expr_Rule, "Multiply Divide order");
      Assert_Unchanged ("2 * (1 + 4)", Expr_Rule, "Multiply Addition order");
      Assert_Unchanged ("not (not v)", Expr_Rule,
                        "brackets need to parse correctly");
   end Test_Mandatory_Parenthesis;

   procedure Test_Removable_Parenthesis (T : in out Test_Case'Class);
   procedure Test_Removable_Parenthesis (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      Assert_Changed
        ("((2))", Expr_Rule,
         "Nested Parenthesis are optional");
      Assert_Changed
        ("f((1))", Expr_Rule,
         "Parenthesis directly in function call are optional");
      Assert_Changed
        ("not (f)", Expr_Rule,
         "Parenthesis are not necessary for not operator");

   end Test_Removable_Parenthesis;

   --  Test plumbing

   overriding function Name
     (T : Minimal_Parenthesis_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Minimal_Parenthesis");
   end Name;

   overriding procedure Register_Tests
     (T : in out Minimal_Parenthesis_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Mandatory_Parenthesis'Access, "Mandatory_Parenthesis");
      Registration.Register_Routine
        (T, Test_Removable_Parenthesis'Access, "Removable_Parenthesis");
   end Register_Tests;

end Test_Minimal_Parenthesis;
