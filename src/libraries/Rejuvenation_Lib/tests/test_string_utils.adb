with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with AUnit.Assertions;          use AUnit.Assertions;
with Rejuvenation.String_Utils; use Rejuvenation.String_Utils;

package body Test_String_Utils is

   Prefix     : constant String := "Prefix";
   New_Prefix : constant String := "New_Prefix";
   Remainder  : constant String := "Remainder";
   Empty      : constant String := "";

   procedure TestCase (Prefix, New_Prefix, Remainder : String) is
   begin

      Assert (Actual   => Replace_Prefix (String_With_Prefix => Prefix & Remainder,
                                          Prefix             => Prefix,
                                          New_Prefix         => New_Prefix),
              Expected => New_Prefix & Remainder,
              Message  => "Substitution failed with" & ASCII.CR & ASCII.LF &
                "Prefix     => " & Prefix     & ASCII.CR & ASCII.LF &
                "New_Prefix => " & New_Prefix & ASCII.CR & ASCII.LF &
                "Remainder  => " & Remainder  & ASCII.CR & ASCII.LF
             );
   end TestCase;

   procedure Test_Replace_Prefix_example (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Prefix, New_Prefix, Remainder);
   end Test_Replace_Prefix_example;

   procedure Test_Replace_Prefix_new_prefix_same_size (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Same_Size_Prefix : constant String := Prefix'Length * "A";
   begin
      Assert (Condition => Same_Size_Prefix'Length = Prefix'Length,
              Message => "Test precondition not realized");
      TestCase (Prefix, Same_Size_Prefix, Remainder);
   end Test_Replace_Prefix_new_prefix_same_size;

   procedure Test_Replace_Prefix_new_prefix_longer (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Longer_Prefix   : constant String := "Longer_Prefix";
   begin
      Assert (Condition => Longer_Prefix'Length > Prefix'Length,
              Message => "Test precondition not realized");
      TestCase (Prefix, Longer_Prefix, Remainder);
   end Test_Replace_Prefix_new_prefix_longer;

   procedure Test_Replace_Prefix_new_prefix_shorter (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Shorter_Prefix  : constant String := "SP";  -- Shorter Prefix
   begin
      Assert (Condition => Shorter_Prefix'Length < Prefix'Length,
              Message => "Test precondition not realized");
      TestCase (Prefix, Shorter_Prefix, Remainder);
   end Test_Replace_Prefix_new_prefix_shorter;

   procedure Test_Replace_Prefix_slices (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Prefix (3 .. 5), New_Prefix (6 .. 8), Remainder (2 .. 4));
   end Test_Replace_Prefix_slices;

   procedure Test_Replace_Prefix_empty_prefix (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Empty, New_Prefix, Remainder);
   end Test_Replace_Prefix_empty_prefix;

   procedure Test_Replace_Prefix_empty_new_prefix (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Prefix, Empty, Remainder);
   end Test_Replace_Prefix_empty_new_prefix;

   procedure Test_Replace_Prefix_empty_remainder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Prefix, New_Prefix, Empty);
   end Test_Replace_Prefix_empty_remainder;

   procedure Test_Replace_Prefix_empty_prefix_and_new_prefix (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Empty, Empty, Remainder);
   end Test_Replace_Prefix_empty_prefix_and_new_prefix;

   procedure Test_Replace_Prefix_empty_prefix_and_remainder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Empty, New_Prefix, Empty);
   end Test_Replace_Prefix_empty_prefix_and_remainder;

   procedure Test_Replace_Prefix_empty_new_prefix_and_remainder (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Prefix, Empty, Empty);
   end Test_Replace_Prefix_empty_new_prefix_and_remainder;

   procedure Test_Replace_Prefix_all_empty (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      TestCase (Empty, Empty, Empty);
   end Test_Replace_Prefix_all_empty;

   procedure
   Test_Replace_Prefix_string_not_start_with_prefix_string_length_larger_than_prefix_length
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert (Actual => Replace_Prefix (String_With_Prefix => "aa",
                                        Prefix             => "b",
                                        New_Prefix         => "c"),
              Expected => "ca",
              Message => "Robustness not as expected.");
   end Test_Replace_Prefix_string_not_start_with_prefix_string_length_larger_than_prefix_length;

   procedure
   Test_Replace_Prefix_string_not_start_with_prefix_string_length_equal_to_prefix_length
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert (Actual => Replace_Prefix (String_With_Prefix => "a",
                                        Prefix             => "b",
                                        New_Prefix         => "c"),
              Expected => "c",
              Message => "Robustness not as expected.");
   end Test_Replace_Prefix_string_not_start_with_prefix_string_length_equal_to_prefix_length;

   --  part of
   --  Test_Replace_Prefix_string_not_start_with_prefix_string_length_smaller_than_prefix_length
   --  located here to prevent: subprogram must not be deeper than access type
   procedure Failing is
      Example : constant String := Replace_Prefix (String_With_Prefix => "a",
                                                   Prefix             => "bb",
                                                   New_Prefix         => "c");
   begin
      pragma Unreferenced (Example);
   end Failing;

   procedure
   Test_Replace_Prefix_string_not_start_with_prefix_string_length_smaller_than_prefix_length
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert_Exception (Failing'Access,
                        "Robustness not as expected.");
   end Test_Replace_Prefix_string_not_start_with_prefix_string_length_smaller_than_prefix_length;

   --  Test plumbing

   overriding function Name
     (T : String_Utils_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("String Utils");
   end Name;

   overriding procedure Register_Tests
     (T : in out String_Utils_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_Replace_Prefix_example'Access, "Example");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_new_prefix_same_size'Access, "New Prefix Same Size");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_new_prefix_longer'Access, "New Prefix Longer");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_new_prefix_shorter'Access, "New Prefix Shorter");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_slices'Access, "Slices");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_empty_prefix'Access, "Empty Prefix");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_empty_new_prefix'Access, "Empty New Prefix");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_empty_remainder'Access, "Empty Remainder");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_empty_prefix_and_new_prefix'Access, "Empty Prefix and New Prefix");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_empty_prefix_and_remainder'Access, "Empty Prefix and Remainder");
      Registration.Register_Routine
        (T,
         Test_Replace_Prefix_empty_new_prefix_and_remainder'Access,
         "Empty New Prefix and Remainder");
      Registration.Register_Routine
        (T, Test_Replace_Prefix_all_empty'Access, "All Empty");
      Registration.Register_Routine
        (T,
         Test_Replace_Prefix_string_not_start_with_prefix_string_length_larger_than_prefix_length
         'Access,
         "Robustness: String not start with prefix. String length larger than prefix length");
      Registration.Register_Routine
        (T,
         Test_Replace_Prefix_string_not_start_with_prefix_string_length_equal_to_prefix_length
         'Access,
         "Robustness: String not start with prefix. String length equal to prefix length");
      Registration.Register_Routine
        (T,
         Test_Replace_Prefix_string_not_start_with_prefix_string_length_smaller_than_prefix_length
         'Access,
         "Robustness: String not start with prefix. String length smaller than prefix length");
   end Register_Tests;

end Test_String_Utils;
