with Ada.Streams;
with Ada.Streams.Stream_IO;
with System;

with Other_Basic_Subprogram_Calls; use Other_Basic_Subprogram_Calls;

package body Task_Subprogram_Calls is

   type R is
      record
         I : Integer;
      end record;

   type S is access R;
   
   procedure R_Output(Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in R);
   
   for R'Output use R_Output;
   
   for S'Storage_Size use Other_F1;
   
   procedure R_Test is
      I : R := (I => 42);
      J : Integer := S'Storage_Size;
      F : Ada.Streams.Stream_IO.File_Type;
      S : Ada.Streams.Stream_IO.Stream_Access;
      N : constant String := "foobar.bin";
   begin
      Ada.Streams.Stream_IO.Create(F, Ada.Streams.Stream_IO.Out_File, N);
      S := Ada.Streams.Stream_IO.Stream(F);
      R'Output(S, I);
   end R_Test;
   
   procedure R_Output(Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in R) is
   begin
      Integer'Output(Stream, Item.I);
   end R_Output;
   
   function FR return access R is
   begin
      return new R;
   end FR;
   
   function F1 return Boolean is
   begin
      return False;
   end F1;

   function F2(B : Boolean) return Boolean is
      I : Integer;
   begin
      I := Integer'First;
      return B;
   end F2;
      
   type ET is (A, B, C);
      
   function F3 return ET is
   begin
      return B;
   end F3;

   procedure Test1 is
      T : T1;
   begin
      T.E1;
   end Test1;
   
   task body T1 is
      I : Integer;
   begin
      accept E1;
      FR.I := 42;
      I := Other_Basic_Subprogram_Calls.Other_F1;
      FR.I := E1'Count;
   exception
      when E : Program_Error =>
         null;
   end T1;

   task body T2 is
   begin
      accept E1;
      
      loop
         select
            when True =>
               accept E1 do
                  Other_Basic_Subprogram_Calls.Other_P1;
               end E1;
         or
            when F1 =>
               accept E2 do
                  Other_Basic_Subprogram_Calls.Other_P1;
               end E2;
         or
            when F2(False) =>
               terminate;
         end select;
      end loop;
   end T2;
   
   procedure Test2 is
      T : T1;
      U : T2;
      
      task T3 is
         entry E1;
         entry E2(I : in out Integer);
         entry E3(1 .. 10);
         entry E4(1 .. 10)(I : Integer);
      end T3;
      
      task body T3 is
      begin
         accept E1;
         accept E2(I : in out Integer) do
            I := I + 1;
         end;
         accept E3(4);
         accept E4(5)(I : Integer);
      end T3;
      
      I : Integer := 42;
      
      procedure E_Rename renames T3.E1;
      
      begin
         T.E1;
         U.E1;
         U.E1;
         U.E2;
         T3.E1;
         T3.E2(I);
         E_Rename;
         T3.E3(5);
         T3.E4(5)(42);
      end Test2;
      
      procedure Test3 is
         P : PT1;
         I : Integer;
      begin
         P.P1;
         P.P2(42);
         I := P.F1;
         I := P.F2(42);
         P.E1;
         P.E2(42);
         P.E4(5);
         P.E5(5)(42);
      end Test3;
      
      protected body PT1 is
         procedure P1 is
         begin
            null;
         end P1;

         procedure P2(I : Integer) is
         begin
            null;
         end P2;

         function F1 return Integer  is
         begin
            return 42;
         end F1;
         
         function F2(I : Integer) return Integer  is
         begin
            return I;
         end F2;

         entry E1 when F2(True) is
         begin
            null;
         end E1;
         
         entry E2(I : Integer) when F1 is
         begin
            null;
         end E2;
         
         entry E4(for J in 1 .. 5) when F1 is
         begin
            null;
         end E4;
         
         entry E5(for J in 1 .. 5)(I : Integer) when True is
            K : Integer := E1'Count;
         begin
            null;
         end E5;

         entry E6 when F2(True) is
         begin
            null;
         end E6;
      end PT1;

      procedure Test4 is
         P : PT1;
         I : Integer;
         S : String := A'Image;
      begin
         P.P1;
         P.P2(42);
         I := P.F1;
         I := P.F2(42);
         P.E1;
         P.E2(42);
         P.E4(5);
         P.E5(5)(42);
      end Test4;

      function Foo return ET renames A;

      type RT(D : Integer) is
      record
         C : Integer;
      end record;

      function F4 return RT is
      begin
         return E : RT(Other_Basic_Subprogram_Calls.Other_F1) do
            E.C := Other_Basic_Subprogram_Calls.Other_F1;
         end return;
      end F4;
      
      procedure P5(R : RT) is
         I : Integer;
      begin
         I := R.D;
      end;
      
   end Task_Subprogram_Calls;
