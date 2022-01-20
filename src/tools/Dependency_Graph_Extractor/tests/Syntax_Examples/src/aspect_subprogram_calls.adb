package body Aspect_Subprogram_Calls is

   procedure P1(I : Integer) is
   begin
      null;
   end P1;

   procedure P2(I : Integer) is
   begin
      null;
   end P2;

   procedure Q1(A : ArrT) is
   begin
      null;
   end Q1;

   procedure Q2a(A : ArrT) is
   begin
      null;
   end Q2a;

   procedure Q2(A : ArrT) is
   begin
      null;
   end Q2;

   procedure Q3(A : in out ArrT) is
   begin
      null;
   end Q3;

   function G return Integer is
   begin
      return 42;
   end G;
   
   function F return String is
   begin
      return G'Image;
   end F;
   
end Aspect_Subprogram_Calls;
