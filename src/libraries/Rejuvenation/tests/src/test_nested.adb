with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions;      use AUnit.Assertions;
with Rejuvenation.Nested;   use Rejuvenation.Nested;

package body Test_Nested is

   OFF_TAG : constant String := "--@OFF@--";
   ON_TAG  : constant String := "--@ON@--";

   procedure Test_None (T : in out Test_Case'Class);
   procedure Test_None (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Input : constant String := "Test";
   begin
      --  pre condition checks
      Assert
        (Condition => Index (Input, ON_TAG) = 0,
         Message   => "ON_TAG unexpectedly in input");
      Assert
        (Condition => Index (Input, OFF_TAG) = 0,
         Message   => "OFF_TAG unexpectedly in input");

      --  test
      Assert
        (Actual   => Remove_Nested_Flags (Input, ON_TAG, OFF_TAG),
         Expected => Input,
         Message  =>
           "Unexpected difference with input " &
           "without any on/off flags - " & Input);
   end Test_None;

   procedure Test_Flat (T : in out Test_Case'Class);
   procedure Test_Flat (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Construction : Unbounded_String  := To_Unbounded_String (OFF_TAG);
      Occurances   : constant Positive := 2;
   begin
      for Index in 1 .. Occurances loop
         Append (Construction, Index'Image & ON_TAG & Index'Image & OFF_TAG);
      end loop;
      Append (Construction, "Tail");

      declare
         Input  : constant String := To_String (Construction);
         Actual : constant String :=
           Remove_Nested_Flags (Input, ON_TAG, OFF_TAG, 1);
      begin
         Assert
           (Actual  => Actual, Expected => Input,
            Message => "Unexpected difference with flat string - " & Input);
      end;
   end Test_Flat;

   procedure Test_Nested (T : in out Test_Case'Class);
   procedure Test_Nested (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Construction_Input, Construction_Expected : Unbounded_String :=
        To_Unbounded_String (OFF_TAG);
      Depth : constant Positive := 2;
   begin
      Append (Construction_Input, "Head");
      Append (Construction_Expected, "Head");

      Append (Construction_Expected, ON_TAG);
      for Index in 1 .. Depth loop
         Append (Construction_Input, ON_TAG & Index'Image);
         Append (Construction_Expected, Index'Image);
      end loop;
      for Index in reverse 1 .. Depth loop
         Append (Construction_Input, Index'Image & OFF_TAG);
         Append (Construction_Expected, Index'Image);
      end loop;
      Append (Construction_Expected, OFF_TAG);

      Append (Construction_Input, "Tail");
      Append (Construction_Expected, "Tail");

      declare
         Input    : constant String := To_String (Construction_Input);
         Expected : constant String := To_String (Construction_Expected);
         Actual   : constant String :=
           Remove_Nested_Flags (Input, ON_TAG, OFF_TAG, 1);
      begin
         Assert
           (Actual  => Actual, Expected => Expected,
            Message => "Unexpected difference with nested string - " & Input);
      end;
   end Test_Nested;

   --  Test plumbing

   overriding function Name (T : Nested_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Nested");
   end Name;

   overriding procedure Register_Tests (T : in out Nested_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_None'Access, "None");
      Registration.Register_Routine (T, Test_Flat'Access, "Flat");
      Registration.Register_Routine (T, Test_Nested'Access, "Nested");
   end Register_Tests;

end Test_Nested;
