package Mismatch is

   function Sum (x, y : Integer) return Integer;

private

   function Sum (x : Integer; y : Integer) return Integer is (x + y);

end Mismatch;
