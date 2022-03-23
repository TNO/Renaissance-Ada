with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Text_Rewrites is

   type Text_Rewrite_Test_Case is
      new Test_Case with null record;

   overriding procedure Register_Tests
     (T : in out Text_Rewrite_Test_Case);
   --  Register routines to be run

   overriding function Name
     (T : Text_Rewrite_Test_Case)
      return Message_String;
   --  Provide name identifying the test case

end Test_Text_Rewrites;
