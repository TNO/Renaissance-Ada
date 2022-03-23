with AUnit.Assertions; use AUnit.Assertions;

package body Missing.AUnit.Assertions is

   procedure Generic_Assert
     (Actual, Expected : Element_T; Message : String;
      Source           : String  := GNAT.Source_Info.File;
      Line             : Natural := GNAT.Source_Info.Line)
   is
   begin
      Assert
        (Condition => Actual = Expected,
         Message   =>
           Message & ASCII.LF & "Actual   : " & Element_T'Image (Actual) &
           ASCII.LF & "Expected : " & Element_T'Image (Expected),
         Source => Source, Line => Line);
   end Generic_Assert;

end Missing.AUnit.Assertions;
