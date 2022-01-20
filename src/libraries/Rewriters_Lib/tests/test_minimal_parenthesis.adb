with AUnit.Assertions;                          use AUnit.Assertions;
with Libadalang.Analysis;                       use Libadalang.Analysis;
with Libadalang.Common;                         use Libadalang.Common;
with Rejuvenation.Simple_Factory;               use Rejuvenation.Simple_Factory;
with Rewriters_Minimal_Parentheses;             use Rewriters_Minimal_Parentheses;

package body Test_Minimal_Parenthesis is

   procedure Assert_Unchanged (Expected : String;
                               Rule : Grammar_Rule;
                               Message : String)
   is
      Unit : constant Analysis_Unit := Analyze_Fragment (Expected, Rule);
      RMP : constant Rewriter_Minimal_Parentheses := Make_Rewriter_Minimal_Parentheses;
      Actual : constant String := RMP.Rewrite (Unit.Root);
   begin
      Assert (Actual, Expected, Message);
   end Assert_Unchanged;

   --  Test Functions

   procedure Test_Mandatory_Brackets (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      Assert_Unchanged ("function f return Integer is (2);", Expr_Fn_Rule, "Brackets are mandatory for Expression Functions");
      Assert_Unchanged ("Code'(Dec)", Expr_Rule, "Brackets are mandatory for Qualified Expressions");
      Assert_Unchanged ("2 * (2/3)", Expr_Rule, "Multiply Divide order");
      Assert_Unchanged ("2 * (1 + 4)", Expr_Rule, "Multiply Addition order");
   end Test_Mandatory_Brackets;

   --  Test plumbing

   overriding function Name
     (T : Minimal_Parenthesis_Test_Case)
      return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Minimal_Parenthesis");
   end Name;

   overriding procedure Register_Tests
     (T : in out Minimal_Parenthesis_Test_Case)
   is
   begin
      Registration.Register_Routine (T, Test_Mandatory_Brackets'Access, "Mandatory_Brackets");
   end Register_Tests;

end Test_Minimal_Parenthesis;
