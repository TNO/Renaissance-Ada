package Prefix_Notation is

   type My_Type is tagged private;

   procedure Operator_Zero (X : My_Type);

   procedure Operator_One (X : My_Type; A : Integer);

private

   type My_Type is tagged null record;

   procedure Operator_Zero (X : My_Type) is null;

   procedure Operator_One (X : My_Type; A : Integer) is null;

end Prefix_Notation;
