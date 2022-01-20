with System;
with Other_Basic_Subprogram_Calls;

package body Generic_Subprogram_Calls is

   G1_Address : System.Address := G1'Address;
   
   generic procedure G1_Renamed renames G1;

   G1_Renamed_Address : System.Address := G1_Renamed'Address;
   
   generic package G4_Renamed renames G4;
   
   procedure Test is
      procedure G1_Instance is new G1(TP => Integer);
      procedure G1_Renamed_Instance is new G1_Renamed(TP => Integer);
      procedure G2_Instance_1 is new G2(PP => Other_Basic_Subprogram_Calls.Other_P1);
      procedure G2_Instance_2 is new G2(G2_Instance_1);
      package G3_Instance is new G3(TP => Integer);
      package G4_Instance is new G4(G2_Instance_1);
      package G4_Renamed_Instance is new G4_Renamed(G2_Instance_2);
      P : access procedure(T : Integer);
      Q : access procedure;
      function G5_Instance is new G5(Integer);
      I : Integer := 42;
      function G6_Instance is new G6(Integer, I);
      PF : access function(T : Integer) return Integer;
      type AQF is access function return Integer;
      subtype AQFS is AQF;
      QF : AQFS;      
   begin
      G1_Instance(42);
      G1_Renamed_Instance(42);
      G2_Instance_1;
      G2_Instance_2;
      P := G1_Instance'Access;
      P := G1_Renamed_Instance'Access;
      P := G3_Instance.P'Access;
      P(42);
      G3_Instance.P(42);
      Q := G2_Instance_2'Access;
      Q.all;
      I := G5_Instance(42);
      I := G6_Instance;
      PF := G5_Instance'Access;
      QF := G6_Instance'Access;
      I := PF(42);
      I := PF.all(42);
      I := QF.all;
      G4_Instance.P;
      G4_Renamed_Instance.P;
   end;

   procedure G1(T : TP) is
      P : access procedure(T : TP);
      X : System.Address := Other_Basic_Subprogram_Calls.Other_P1'Address;
   begin
      Other_Basic_Subprogram_Calls.Other_P1;
      G1(T);
      P := G1'Access;
   end G1;
   
   procedure G2 is
      X : System.Address;
   begin
      PP;
      X := PP'Address;
      X := G2'Address;
   end G2;
   
   package body G3 is
      procedure P(T : TP) is
         X : System.Address := P'Address;
      begin
         Other_Basic_Subprogram_Calls.Other_P1;
         P(T);
      end P;
   end G3;

   package body G4 is
      procedure P is
         X : System.Address := PP'Address;
      begin
         PP;
         P;
         X := P'Address;
      end P;
   end G4;
   
   function G5(T : TP) return TP is
   begin
      return T;
   end G5;

   function G6 return TP is
   begin
      return T;
   end G6;
   
   function Equal_Generic(L, R : T) return Boolean is
   begin
      return L = R;
   end Equal_Generic;
   
   function Equal_Bool_1 is new Equal_Generic(Boolean, Standard."=");

   function Equal_Bool_2 is new Equal_Generic(Boolean);

   function Foo_Generic(L, R : T) return Boolean is
   begin
      return Foo(L, R);
   end Foo_Generic;
   
   function Foo(L, R : Boolean) return Boolean is
   begin
      return L = R;
   end Foo;
   
   function Foo_Bool_1 is new Foo_Generic(Boolean, Foo);

   function Foo_Bool_2 is new Foo_Generic(Boolean);

   package Equal_Bool_Package_1 is new Equal_Generic_Package(Boolean, Standard."=");

   package Equal_Bool_Package_2 is new Equal_Generic_Package(Boolean);

   package Foo_Bool_Package_1 is new Foo_Generic_Package(Boolean, Foo);

   package Foo_Bool_Package_2 is new Foo_Generic_Package(Boolean);
      
end Generic_Subprogram_Calls;
