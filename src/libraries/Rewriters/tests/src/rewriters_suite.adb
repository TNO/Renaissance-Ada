with AUnit.Simple_Test_Cases;  use AUnit.Simple_Test_Cases;
with Test_Minimal_Parenthesis; use Test_Minimal_Parenthesis;

package body Rewriters_Suite is

   function Suite return Access_Test_Suite is
      Ret                 : constant Access_Test_Suite := new Test_Suite;
      Minimal_Parenthesis : constant Test_Case_Access  :=
        new Minimal_Parenthesis_Test_Case;

   begin
      Ret.Add_Test (Minimal_Parenthesis);
      return Ret;
   end Suite;

end Rewriters_Suite;
