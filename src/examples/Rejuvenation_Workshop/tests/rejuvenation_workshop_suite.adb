with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Test_Examples;           use Test_Examples;
with Test_Exercises_Intro;    use Test_Exercises_Intro;
with Test_Exercises_Navigate; use Test_Exercises_Navigate;
with Test_Exercises_Match;    use Test_Exercises_Match;
with Test_Exercises_Rewrite;  use Test_Exercises_Rewrite;

package body Rejuvenation_Workshop_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
      WTC : constant Test_Case_Access := new Example_Test_Case;
      EITC : constant Test_Case_Access := new Exercise_Intro_Test_Case;
      ENTC : constant Test_Case_Access := new Exercise_Navigate_Test_Case;
      EMTC : constant Test_Case_Access := new Exercise_Match_Test_Case;
      ERTC : constant Test_Case_Access := new Exercise_Rewrite_Test_Case;
   begin
      Ret.Add_Test (WTC);
      Ret.Add_Test (EITC);
      Ret.Add_Test (ENTC);
      Ret.Add_Test (EMTC);
      Ret.Add_Test (ERTC);
      return Ret;
   end Suite;

end Rejuvenation_Workshop_Suite;
