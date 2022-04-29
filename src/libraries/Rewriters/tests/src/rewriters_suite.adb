with AUnit.Simple_Test_Cases;  use AUnit.Simple_Test_Cases;
with Test_Minimal_Parenthesis; use Test_Minimal_Parenthesis;
with Test_Node_Interval;     use Test_Node_Interval;

package body Rewriters_Suite is

   function Suite return Access_Test_Suite is
      Ret                 : constant Access_Test_Suite := new Test_Suite;
      Minimal_Parenthesis : constant Test_Case_Access  :=
        new Minimal_Parenthesis_Test_Case;
      Node_Boundaries : constant Test_Case_Access  :=
        new Node_Interval_Test_Case;

   begin
      Ret.Add_Test (Minimal_Parenthesis);
      Ret.Add_Test (Node_Boundaries);
      return Ret;
   end Suite;

end Rewriters_Suite;
