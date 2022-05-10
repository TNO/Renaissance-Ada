--  Interval specifies an integer range.
--  Range is a reserved Ada keyword.

--  Although in some cases (prepend / append) only one of the boundaries
--  of the interval is needed, we calculate and store both.

package Intervals is

   type Interval is tagged private;

   function Make_Interval (First : Positive; Last : Natural) return Interval;
   --  Constructor for Interval

   function Is_Empty (I : Interval) return Boolean;
   --  Is the interval empty?

   function Length (I : Interval) return Natural;
   --  The length of the interval

   function Before (I_A, I_B : Interval) return Boolean with
      Pre => not Is_Empty (I_A) and then not Is_Empty (I_B);
      --  Is interval I_A before interval I_B?

   function After (I_A, I_B : Interval) return Boolean with
      Pre => not Is_Empty (I_A) and then not Is_Empty (I_B);
      --  Is interval I_A after interval I_B?
      --  Note: Before (I_A, I_B) = After (I_B, I_A)

   function Contains (I_A, I_B : Interval) return Boolean with
      Pre => not Is_Empty (I_A) and then not Is_Empty (I_B);
      --  Does interval I_A contain interval I_B?

   function Overlaps (I_A, I_B : Interval) return Boolean with
      Pre => not Is_Empty (I_A) and then not Is_Empty (I_B);
      --  Does interval I_A overlap with interval I_B?
      --  Note: Overlaps (I_A, I_B) = Overlaps (I_B, I_A)

   function Image (I : Interval) return String;

private

   type Interval is tagged record
      F_First : Positive;
      F_Last  : Natural;
   end record;

   function Make_Interval (First : Positive; Last : Natural) return Interval is
     (First, Last);

   function Is_Empty (I : Interval) return Boolean is (I.F_Last < I.F_First);

   function Length (I : Interval) return Natural is
     (if I.F_Last < I.F_First then 0 else I.F_Last - I.F_First + 1);

   function Before (I_A, I_B : Interval) return Boolean is
     (I_A.F_Last < I_B.F_First);

   function After (I_A, I_B : Interval) return Boolean is
     (I_A.F_First < I_B.F_Last);

   function Contains (I_A, I_B : Interval) return Boolean is
     (I_A.F_First <= I_B.F_First and then I_B.F_Last <= I_A.F_Last);

   function Overlaps (I_A, I_B : Interval) return Boolean is
     (I_B.F_First <= I_A.F_Last and then I_A.F_First <= I_B.F_Last);
   --  See https://stackoverflow.com/questions/
   --     325933/determine-whether-two-date-ranges-overlap
   --     3269434/whats-the-most-efficient-way-to-test-if-two-ranges-overlap

   function Image (I : Interval) return String is
      (I.F_First'Image & "-" & I.F_Last'Image);
end Intervals;
