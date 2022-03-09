with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

package Rejuvenation.String_Utils with
   SPARK_Mode => On
is

   function Starts_With (A_String, Beginning : String) return Boolean is
     (A_String'Length >= Beginning'Length
      and then Head (A_String, Beginning'Length) = Beginning);
   --  Does the given string start with the given beginning?

   function Starts_with_Case_Insensitive
     (A_String, Beginning : String) return Boolean is
     (Starts_With (To_Lower (A_String), To_Lower (Beginning)));
   --  Does the given string start with the given beginning,
   --  independent of the case?

   function Ends_With (A_String, Ending : String) return Boolean is
     (A_String'Length >= Ending'Length
      and then Tail (A_String, Ending'Length) = Ending);
   --  Does the given string end with the given ending?
   --  GNATCOLL.Strings also provides an Ends_With that needs
   --  an XString [yet another string class ;-( ]

   function Ends_with_Case_Insensitive
     (A_String, Ending : String) return Boolean is
     (Ends_With (To_Lower (A_String), To_Lower (Ending)));
--  Does the given string end with the given ending (independent of the case)?

   function Replace_Prefix
     (String_With_Prefix : String; Prefix, New_Prefix : String)
      return String with
      Pre =>
      --  Length constraints (added to help Prover)
      String_With_Prefix'Length >= Prefix'Length
      and then
      --  Empty string cannot be [Postive'Last + 1 .. Positive'Last]
      --  due to overflow hence
      String_With_Prefix'First <= Positive'Last - Prefix'Length
      and then New_Prefix'First <= Positive'Last - New_Prefix'Length
      and then
      --  Overflow protection, since the length of a string is limited by
      --  the range of Natural [0 .. Natural'Last]
      --  Furthermore, New_Prefix'First >= 1, so not whole range is
      --  necessarily available
      --  [type String is array (Positive range <>) of Character
      --    + with exceptions for empty string]

        To_Big_Integer (String_With_Prefix'Length) -
          To_Big_Integer (Prefix'Length) <=
        To_Big_Integer (Positive'Last) - To_Big_Integer (New_Prefix'Last)
      and then
      --  Value constraints

        String_With_Prefix
          (String_With_Prefix'First ..
               String_With_Prefix'First + (Prefix'Length - 1)) =
        Prefix,
      Post =>
      --  Value Constraints
      Replace_Prefix'Result
        (Replace_Prefix'Result'First ..
             Replace_Prefix'Result'First + (New_Prefix'Length - 1)) =
      New_Prefix and then
      Replace_Prefix'Result
          (Replace_Prefix'Result'First + New_Prefix'Length ..
               Replace_Prefix'Result'Last) =
        String_With_Prefix
          (String_With_Prefix'First + Prefix'Length ..
               String_With_Prefix'Last),

      Test_Case => ("Example", Nominal),
      Test_Case => ("New Prefix Same Size", Nominal),
      Test_Case => ("New Prefix Longer", Nominal),
      Test_Case => ("New Prefix Shorter", Nominal),
      Test_Case => ("Slices", Nominal),
      Test_Case => ("Empty Prefix", Nominal),
      Test_Case => ("Empty New Prefix", Nominal),
      Test_Case => ("Empty Remainder", Nominal),
      Test_Case => ("Empty Prefix and New Prefix", Nominal),
      Test_Case => ("Empty Prefix and Remainder", Nominal),
      Test_Case => ("Empty New Prefix and Remainder", Nominal),
      Test_Case => ("All Empty", Nominal),
      Test_Case => ("String not start with Prefix, "
                    & "Strlen larger than Prefix length",
                    Robustness),
      Test_Case => ("String not start with Prefix, "
                    & "Strlen equal to Prefix length",
                    Robustness),
      Test_Case => ("String not start with Prefix, "
                    & "Strlen smaller than Prefix length",
                    Robustness);
      --  Replace 'Prefix' with 'New_Prefix' in 'String_With_Prefix'.
      --  @param String_With_Prefix String that starts with Prefix
      --  @param Prefix Prefix present in String_With_Prefix to be replaced
      --  by New_Prefix
      --  @param New_Prefix New Prefix to replace Prefix in String_With_Prefix
      --  @return Given that String_With_Prefix = Prefix & Remainder return is
      --  equal to New_Prefix & Remainder

   function Replace_All (Source, Pattern, Replacement : String) return String;
   --  Replace all occurrences of Pattern in Source by the given Replacement.
   --
   --  Note: Search order is left to right,
   --  matched parts are replace as a whole.
   --
   --  Example: Replace_All ("InInInInIn, "InIn", "Out") = "OutOutIn"

end Rejuvenation.String_Utils;
