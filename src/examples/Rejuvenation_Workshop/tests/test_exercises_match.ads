with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Exercises_Match is

   type Exercise_Match_Test_Case is
      new Test_Case with null record;

   overriding procedure Register_Tests
     (T : in out Exercise_Match_Test_Case);
   --  Register routines to be run

   overriding function Name
     (T : Exercise_Match_Test_Case)
      return Message_String;
   --  Provide name identifying the test case

end Test_Exercises_Match;
