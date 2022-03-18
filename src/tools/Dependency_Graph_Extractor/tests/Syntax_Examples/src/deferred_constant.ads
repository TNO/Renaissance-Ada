package Deferred_Constant is

   I, J : constant Integer;

   --  K, L : Integer;
   
private

   I : constant Integer := 42;

   J : constant Integer := 42;
   
end Deferred_Constant;
pragma Pure (Deferred_Constant);
