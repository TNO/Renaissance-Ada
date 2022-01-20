package body Test_Operator_Attribute is

   type Rec is
   record
      I : Integer := 0;
   end record;
   
   function Sum(L, R : Rec) return Integer is
     (R.I + L.I);

   function "+"(L, R : Rec) return Integer is
     (R.I + L.I);
   
   procedure Test is
      Foo : access function(L, R : Rec) return Integer := Sum'Access;
      Bar : access function(L, R : Rec) return Integer := "+"'Access;
      R, L : Rec;
      I : Integer;
   begin
      Bar := "+"'Access;
      I := R + L;
      I := "+"(R, L);
   end Test;

end Test_Operator_Attribute;
