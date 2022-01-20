with Other_Basic_Subprogram_Calls;

package Tagged_Subprogram_Calls is

   package Inner is
      type E is (A, B, C);
      
      procedure P(A : E);
   end Inner;

   procedure Test(Param : Inner.E);
   
   type T is tagged
     null record;

   procedure T_P(S : T);

   procedure T_Q(J : Integer; S : T);

   procedure T_R(S : T; J : Integer);
   
   function T_F(S : T) return T'Class;

   function T_G(S : T) return T;
   
   function "+"(S : T) return Boolean;
     
   type T1 is new T with
   record
      I : Integer;
   end record;
   
   overriding procedure T_P(S : T1);
   
   overriding procedure T_Q(J : Integer; S : T1);

   procedure T_R(S : T1; J : Integer);

   overriding function T_F(S : T1) return T'Class;

   function T_G(S : T1) return T1;

   function "+"(S : T1) return Boolean;
   
   type T2 is new T1 with
      null record;
   
   procedure T_P(S : T2);
   
   procedure Test2;

   type S is abstract tagged
     null record;
   
   procedure S_P(A : S) is abstract;
   
   type I is interface;
   
   procedure S_I(A : I) is abstract;
   
   type I1 is new I with
     null record;
   
   procedure S_I(A : I1);
   
   type S1 is new S and I with
     null record;
   
   procedure S_P(A : S1);
   
   procedure S_I(A : S1);
   
   type S2 is new S1 with
     null record;
   
   procedure S_P(A : S2);
   
   procedure S_I(A : S2);
   
   procedure Test3;
   
   generic
      type TP is abstract tagged private;
      with procedure PP(I : TP) is abstract;
   procedure GP1(I : TP'Class);

   generic
      type TP is abstract tagged private;
      with procedure PP(I : TP) is abstract;
   procedure GP2(I : TP'Class);
   
   type J is limited interface;
   
   task TI is
        new J with
      entry E;
   end TI;

   type K is limited interface;
   
   task type TIT is
        new K with
      entry E;
   end TIT;

   type L is limited interface;
   
   protected PI is
        new L with
      entry E;
   end PI;

   type M is limited interface;
   
   protected type PIT is
        new M with
      entry E;
   end PIT;
   
private
   function T_F(S : T) return T'Class is
      (S);

   function T_G(S : T) return T is
      (S);

   function "+"(S : T) return Boolean is
      (True);

   function T_F(S : T1) return T'Class is
      (S);

   function T_G(S : T1) return T1 is
      (S);

   function "+"(S : T1) return Boolean is
      (False);
   
end Tagged_Subprogram_Calls;
