package body Operator_Subprogram_Calls is
   
   type R is
      record
         I : Integer;
      end record;

   function "<"(P1 : R; P2 : R) return Boolean;
   
   procedure Test is
      R1 : R := (I => 42);
      R2 : R := (I => 42);
      function Foo(I1 : R; I2 : R) return Boolean renames "<";
   begin
      if (R1 < R2) and Foo(R1, R2) then
         null;
      end if;
   end Test;

   function "<"(P1 : R; P2 : R) return Boolean is
      function Foo(I1 : Integer; I2 : Integer) return Boolean renames "<";
   begin
      return Foo(P1.I, P2.I);
   end "<";
   
   function "<="(P1 : R; P2 : R) return Boolean;
   
   procedure Test2 is
      R1 : R := (I => 42);
      R2 : R := (I => 42);
      function Foo(I1 : R; I2 : R) return Boolean renames "<=";
   begin
      if (R1 <= R2) and Foo(R1, R2) then
         null;
      end if;
   end Test2;

   function "<="(P1 : R; P2 : R) return Boolean is
      function Foo(I1 : Integer; I2 : Integer) return Boolean renames "<=";
   begin
      return Foo(P1.I, P2.I);
   end "<=";
   
end Operator_Subprogram_Calls;
