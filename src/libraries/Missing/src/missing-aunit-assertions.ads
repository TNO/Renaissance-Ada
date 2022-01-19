with GNAT.Source_Info;

package Missing.AUnit.Assertions is

   --  See https://github.com/AdaCore/aunit/issues/31
   generic
      type Element_T is (<>);
   procedure Generic_Assert
     (Actual, Expected : Element_T; Message : String;
      Source           : String  := GNAT.Source_Info.File;
      Line             : Natural := GNAT.Source_Info.Line);
   --  Specialized versions of Assert, they call the general version that
   --  takes a Condition as a parameter

end Missing.AUnit.Assertions;
