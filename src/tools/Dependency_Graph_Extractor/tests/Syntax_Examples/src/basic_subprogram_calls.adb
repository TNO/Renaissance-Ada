with Basic_Subprogram_Calls.Child;
with Other_Basic_Subprogram_Calls;
with Subprogram_Unit;
with Subprogram_Unit_2;

package body Basic_Subprogram_Calls is

   function F7 return Integer;
   
   function F8 return Integer is
   begin
      return 42;
   end F8;
   
   function F9 return Integer;
   
   function F9 return Integer is
   begin
      return 42;
   end F9;

   function F12 return Integer is separate;

   procedure P4 is separate;
   
   function F13(I : Integer) return Integer renames F2;

   function F14 return Integer renames F12;
   
   procedure P5(I : Integer := F14) renames P3;
   
   procedure P6 renames P4;
   
   procedure Test is
      I : Integer := F10;
   begin
      I := F1;
      I := F2(42);
      I := F3;
      I := F3(42);
      I := F4;
      I := F5(42);
      I := F6;
      I := F6(42);
      I := F7;
      I := F8;
      I := F9;
      
      declare
         J : Integer;
      begin
         J := F1;
      end;
      
      Label:
      declare
         J : Integer;
      begin
         J := F2(F12);
      end Label;
      
      P1;
      P2(42);
      P3;
      P3(F11);
      P4;
   end Test;

   function F1 return Integer is
   begin
      return 42;
   end F1;
   
   function F2(I : Integer) return Integer is
   begin
      return I;
   end F2;
   
   function F3(I : Integer := 42) return Integer is
   begin
      return I;
   end F3;

   function F7 return Integer is
   begin
      return 42;
   end F7;
   
   procedure P1 is
   begin
      null;
   end P1;
   
   procedure P2(I : Integer) is
   begin
      null;
   end P2;

   procedure P3(I : Integer := F1) is
      J : Integer;
   begin
      J := F13(I);
      J := F14;
      P5;
      P5(J);
      P6;
      J := Nested.Nested_F1;
      Nested.Nested_P1;
      
      declare
         use Nested;
      begin
         J := Nested.Nested_F1;
         Nested.Nested_P1;
      end;
      
      P7;
      P8;
      
      J := Child.Child_F1;
      Child.Child_P1;
      J := Other_Basic_Subprogram_Calls.Other_F1;
      Other_Basic_Subprogram_Calls.Other_P1;
      
      declare
         use Child;
         use Other_Basic_Subprogram_Calls;
      begin
         J := Child_F1;
         Child_P1;
         J := Other_F1;
         Other_P1;
      end;
   end P3;
   
   package body Nested is

      function Nested_F1 return Integer is
      begin
         return 42;
      end Nested_F1;
      
      procedure Nested_P1 is
         I : Integer := 42;
         S : String := I'Image;
         F : access function return Integer := Other_Basic_Subprogram_Calls.Other_F1'Access;
         P : access procedure := Other_Basic_Subprogram_Calls.Other_P1'Access;
      begin
         P := Nested_P1'Access;
         P := Nested_P1'Access;
         P := P1'Access;
         P := P7'Access;
         F := F1'Access;
         F := F4'Access;
         F := F14'Access;
         F := F12'Access;
      end Nested_P1;
       
   end Nested;
   
   package Nested_Renamed renames Nested;
   
   procedure P8 is null;

   function F_Overload(I: Integer) return Integer is
   begin
      return I;
   end F_Overload;
      
   function F_Overload(B: Boolean) return Integer is
   begin
      return 42;
   end F_Overload;
   
   function F_Overload(I: Integer) return Boolean is
   begin
      return True;
   end F_Overload;

   procedure P_Overload(I : Integer) is
   begin
      null;
   end P_Overload;

   procedure P_Overload(B : in out Boolean) is
      I : Integer;
   begin
      B := F_Overload(42);
      I := F_Overload(B);
      I := F_Overload(I);
      P_Overload(B);
      P_Overload(I);
      Nested_Renamed.Nested_P1;
      I := Nested_Renamed.Nested_F1;
   end P_Overload;

   procedure Test3 is
      A : array (1 .. 5) of Integer;
      
      type PAT is array (Integer range <>) of access procedure;
      
      function F1 return access PAT is
      begin
         return new PAT(1 .. 5);
      end F1;

      function F2(I : Integer) return access PAT is
      begin
         return new PAT(1 .. I);
      end F2;
      
   begin
      A(3) := 42;
      F1(2) := Other_Basic_Subprogram_Calls.Other_P1'Access;
      F2(42)(2) := Other_Basic_Subprogram_Calls.Other_P1'Access;
      F2(42)(2) := F1(3);
      A(2) := A(3);
   end Test3;
   
   function F_Internal return Integer is separate;
   
   procedure Test4 is
      Exception_Declaration_Name : exception;
   begin
      Statement_Identifier_Name:
      begin
         Test3;
      exception
         when Choice_Parameter_Specification_Name: Exception_Declaration_Name =>
            raise;
      end Statement_Identifier_Name;
      
      <<Label_Statement_Identifier_Name>> Test3;
   end Test4;
   
   X : aliased Integer := F9;

   Y : array (1 .. 5) of access Integer;
   
   procedure AnonSubp(F : access function(X, Y: Integer) return Integer) is null;
   
   function AnonReturn return access Integer is
   begin
      return X'Access;
   end AnonReturn;

begin
   Subprogram_Unit;
   Subprogram_Unit_2;
end Basic_Subprogram_Calls;
