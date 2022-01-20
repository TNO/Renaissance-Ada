with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Examples is

   type Example_Test_Case is
      new Test_Case with null record;

   overriding procedure Register_Tests
     (T : in out Example_Test_Case);
   --  Register routines to be run

   overriding function Name
     (T : Example_Test_Case)
      return Message_String;
   --  Provide name identifying the test case

end Test_Examples;
