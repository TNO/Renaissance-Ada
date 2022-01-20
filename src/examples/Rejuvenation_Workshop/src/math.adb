package body Math is

   procedure Temp (x : Integer)
   is
     Square : constant Integer := x * x;
   begin
      pragma Unreferenced (Square);
   end Temp;

end Math;
