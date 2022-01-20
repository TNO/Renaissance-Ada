package Task_Subprogram_Calls is

   task type T1 is
      entry E1;
   end T1;

   task type T2 is
      entry E1;
      entry E2;
   end T2;

   procedure Test1;

   procedure Test2;
   
   protected type PT1 is
      procedure P1;
      procedure P2(I : Integer);
      function F1 return Integer;
      function F2(I : Integer) return Integer;
      entry E1;
      entry E2(I : Integer);
      entry E4(1 .. 5);
      entry E5(1 .. 5)(I : Integer);

      private
      entry E6;
   end PT1;
   
end Task_Subprogram_Calls;
