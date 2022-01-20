package body Test_Call_Filtering is

   type T is tagged
     null record;

   function P(Self : T) return T is
   begin
      return Self;
   end P;
   
   function Q(Self : T; I : Integer) return T is
   begin
      return Self;
   end Q;
      
   procedure Test is
      E : T;
   begin
      E := E.P.P;
      E := E.Q(1).Q(2);
   end Test;

end Test_Call_Filtering;
