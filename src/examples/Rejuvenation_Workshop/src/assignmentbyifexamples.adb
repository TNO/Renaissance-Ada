with Ada.Text_IO; use Ada.Text_IO;

package body AssignmentByIfExamples is

   function Example1_Pattern1 (condition : Boolean) return Boolean;
   function Example1_Pattern1 (condition : Boolean) return Boolean is
      variable : Boolean;
   begin
      if condition then
         variable := True;
      else
         variable := False;
      end if;
      return variable;
   end Example1_Pattern1;

   function Example2_Pattern1 (condition : Boolean) return Boolean;
   function Example2_Pattern1 (condition : Boolean) return Boolean is
      AB : array (1 .. 1) of Boolean;
   begin
      if not condition then
         AB (1) := True;
      else
         AB (1) := False;
      end if;
      return AB (1);
   end Example2_Pattern1;

   function Example_Pattern2 (condition : Boolean) return Boolean;
   function Example_Pattern2 (condition : Boolean) return Boolean is
      variable : Boolean;
   begin
      variable := False;
      if condition then
         variable := True;
      end if;
      return variable;
   end Example_Pattern2;

   function Example_Pattern3 (condition : Boolean) return Boolean;
   function Example_Pattern3 (condition : Boolean) return Boolean is
      variable : Boolean := False;
   begin
      if condition then
         variable := True;
      end if;
      return variable;
   end Example_Pattern3;

   procedure Dummy is
   begin
      if Example1_Pattern1 (True) and then
        Example2_Pattern1 (True) and then
        Example_Pattern2 (True) and then
        Example_Pattern3 (True)
      then
         Put_Line ("True");
      end if;
   end Dummy;

end AssignmentByIfExamples;
