package Forward_Declaration is

   X : constant Integer;

   procedure P;

   type T is private;

private
   
   X : constant Integer := 5;

   type T is new Integer;
   
end Forward_Declaration;
