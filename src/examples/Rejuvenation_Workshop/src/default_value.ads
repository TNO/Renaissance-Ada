package Default_Value is

   function My_Function (X : Integer; Y : Integer := 2) return Integer;

private

   function My_Function (X : Integer; Y : Integer := 2) return Integer is
     (X + Y);

end Default_Value;
