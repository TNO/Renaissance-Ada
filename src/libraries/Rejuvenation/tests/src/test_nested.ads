with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Nested is

   type Nested_Test_Case is
      new Test_Case with null record;

   overriding procedure Register_Tests
     (T : in out Nested_Test_Case);
   --  Register routines to be run

   overriding function Name
     (T : Nested_Test_Case)
      return Message_String;
   --  Provide name identifying the test case

end Test_Nested;
