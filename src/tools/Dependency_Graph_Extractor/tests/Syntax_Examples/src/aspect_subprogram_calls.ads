with Other_Basic_Subprogram_Calls;

package Aspect_Subprogram_Calls is

   type ArrT is array (Integer range <>) of Integer;
   
   type ArrS is private
     with Type_Invariant => Check(ArrS) and Other_Basic_Subprogram_Calls.Other_F1 = 42;
   
   function Check(A : ArrS) return Boolean;
   
   procedure P1(I : Integer)
     with Pre => I = Other_Basic_Subprogram_Calls.Other_F1;

   procedure P2(I : Integer)
     with Pre => (if Other_Basic_Subprogram_Calls.Other_F1 = 42 then True else I = Other_Basic_Subprogram_Calls.Other_F1);

   function F(A : ArrT) return Integer is
      (A'Last);

   function G return Integer
     with Post => (G'Result = Other_Basic_Subprogram_Calls.Other_F1);

   procedure Q1(A : ArrT)
     with
       Pre => (for all I in A'First .. F(A) => A(I) = Other_Basic_Subprogram_Calls.Other_F1);

   procedure Q2a(A : ArrT)
     with
       Pre => (True and True);
   
   procedure Q2(A : ArrT)
     with
       Pre => ((for all I in A'First .. F(A) => A(I) = Other_Basic_Subprogram_Calls.Other_F1)
       and (for some J in A'First .. F(A) => A(J) = Other_Basic_Subprogram_Calls.Other_F1));

   procedure Q3(A : in out ArrT)
     with
       Post => (for all I in A'First .. F(A) => A(I) = A'Old(I) + Other_Basic_Subprogram_Calls.Other_F1);

   type R is record
      I : Integer;
   end record
     with Dynamic_Predicate => I > Other_Basic_Subprogram_Calls.Other_F1;
   
   type F_Enum is (A, B, C)
     with Convention => Fortran;

   type C_Enum is (A, B)
     with Convention => C;
   
private
   
   type ArrS is record
      A : Integer;
   end record;

   function Check(A : ArrS) return Boolean is
     (True);
   
end Aspect_Subprogram_Calls;
