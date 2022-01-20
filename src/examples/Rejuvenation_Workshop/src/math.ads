package Math is

   function Square (x : Integer) return Integer;

   function Exponent (base, power : Integer) return Integer;

   type Matrix_Type is
     array (Positive range <>, Positive range <>) of Integer;

   function Square (m : Matrix_Type) return Matrix_Type;

end Math;
