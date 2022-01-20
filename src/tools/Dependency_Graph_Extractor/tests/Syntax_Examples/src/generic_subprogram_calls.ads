package Generic_Subprogram_Calls is

   generic
      type TP is private;
   procedure G1(T : TP);

   generic
      with procedure PP;
   procedure G2;
   
   generic
      type TP is private;
   package G3 is
      procedure P(T : TP);
   end G3;

   generic
      with procedure PP;
   package G4 is
      procedure P;
   end G4;
   
   generic
      type TP is private;
   function G5(T : TP) return TP;

   generic
      type TP is private;
      T : TP;
   function G6 return TP;

   procedure Test;
   
   generic
      type T is private;
      with function "="(L, R : T) return Boolean is <>;
   function Equal_Generic(L, R : T) return Boolean;

   generic
      type T is private;
      with function Foo(L, R : T) return Boolean is <>;
   function Foo_Generic(L, R : T) return Boolean;
   
   generic
      type T is private;
      with function "="(L, R : T) return Boolean is <>;
   package Equal_Generic_Package is
   end Equal_Generic_Package;
     
   generic
      type T is private;
      with function Foo(L, R : T) return Boolean is <>;
   package Foo_Generic_Package is
   end Foo_Generic_Package;
   
end Generic_Subprogram_Calls;
