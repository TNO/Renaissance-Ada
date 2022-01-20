with Ada.Text_IO;

package body Tagged_Subprogram_Calls is

   package body Inner is
      procedure P(A : E) is
      begin
         null;
      end P;
   end Inner;         

   procedure Test(Param : Inner.E) is
      type F is new Inner.E range Param .. Inner.C;
      
      O : F := B;
      Q : Inner.E := Inner.B;
   begin
      P(O);
      Inner.P(Q);
   end Test;
      
   procedure Test2 is
      A : T;
      B : T1 := (I => 42);
      C : T'Class := A;
      D : T'Class := B;
      E : T1'Class := B;
      F : T'Class := T_F(D);
   begin
      T_P(A);
      T_P(B);
      T_P(C);
      T_P(D);
      T_P(E);
      T_Q(42, A);
      T_Q(42, B);
      T_Q(42, C);
      T_Q(42, D);
      T_Q(42, E);
      A.T_P;
      B.T_P;
      C.T_P;
      D.T_P;
      E.T_P;
      A.T_R(42);
      B.T_R(42);
      C.T_R(42);
      D.T_R(42);
      E.T_R(42);
      D.T_F.T_P;
      B.T_F.T_P;
      D.T_F.T_R(42);
      B.T_F.T_R(42);
      D.T_G.T_R(42); -- D.T_G should yield class wide type, but doesn't
      B.T_G.T_R(42);
      
      if +D then
         null;
      end if;
   end Test2;

   procedure T_P(S : T) is null;
   
   procedure T_P(S : T1) is null;

   procedure T_P(S : T2) is null;

   procedure T_Q(J : Integer; S : T) is null;
   
   procedure T_Q(J : Integer; S : T1) is null;

   procedure T_R(S : T; J : Integer) is
   begin
      Ada.Text_IO.Put_Line("T");
   end T_R;

   procedure T_R(S : T1; J : Integer) is 
   begin
      Ada.Text_IO.Put_Line("T1");
   end T_R;

   procedure Test3 is
      O1 : S1;
      O2 : S'Class := O1;
      O3 : I'Class := O1;
   begin
      O1.S_P;
      O2.S_P;
      O1.S_I;
      O3.S_I;
   end Test3;

   procedure S_I(A : I1) is
   begin
      null;
   end S_I;

   procedure S_P(A : S1) is
   begin
      Ada.Text_IO.Put_Line("S1");
   end S_P;

   procedure S_I(A : S1) is
   begin
      Ada.Text_IO.Put_Line("S1");
   end S_I;

   procedure S_P(A : S2) is
   begin
      Ada.Text_IO.Put_Line("S2");
   end S_P;

   procedure S_I(A : S2) is
   begin
      Ada.Text_IO.Put_Line("S2");
   end S_I;
   
   procedure GP1(I : TP'Class) is
   begin
      PP(I);
   end GP1;

   procedure GPI is new GP1(TP => S, PP => S_P);

   procedure GP2(I : TP'Class) is
      procedure GPI is new GP1(TP => TP, PP => PP);
   begin
      null;
   end GP2;
   
   task body TI is
   begin
      accept E;
   end TI;

   task body TIT is
   begin
      accept E;
   end TIT;
   
   protected body PI is
      entry E when True is
      begin
         null;
      end E;
   end PI;
   
   protected body PIT is
      entry E when True is
      begin
         null;
      end E;
   end PIT;
   
end Tagged_Subprogram_Calls;
