package Count_Subprogram is

   procedure P2A (x, y : Integer);
   procedure P2B (x : Integer; y : Integer);

   procedure P4A (k, l, m, n : Integer);
   procedure P4B (k : Integer; l : Integer; m : Integer; n : Integer);

   procedure P3A (x, y, z : Integer);
   procedure P3B (x, y : Integer; z : Integer);
   procedure P3C (x : Integer; y, z : Integer);
   procedure P3D (x : Integer; y : Integer; z : Integer);

   procedure P3E (x, y, z : Integer) is null;
   procedure P3F (x : Integer; y : Integer; z : Integer) is null;

   procedure P3G (x : Integer  := 0; y : Integer  := 1; z : Integer  := 2);
   procedure P3H (x, y, z : Integer  := 0);

   procedure P3I (x, y, z : in Integer);
   procedure P3J (x, y, z : in out Integer);
   procedure P3K (x, y, z : out Integer);

   procedure P3L (x, y, z : Integer) renames P3A;
   procedure P3M (a, b, c : Integer) renames P3A;

   generic
      type Element_T is private;
   procedure P3N (x, y, z : Element_T);

   generic
      type Element_T is private;
   procedure P3O (x : Element_T; y : Element_T; z : Element_T);

   generic
      with procedure P3P (x, y, z : Integer);
      with procedure P3Q (x : Integer; y : Integer; z : Integer);
   package My_Package is
   end My_Package;

   type Callback_Procedure_A is access procedure (x, y, z : Integer);
   type Callback_Procedure_B is access procedure (x : Integer; y : Integer; z : Integer);

   procedure S1 (Call_Back : access procedure (x, y, z : Integer));
   procedure S2 (Call_Back : access procedure (x : Integer; y : Integer; z : Integer));

   function F3A (x, y, z : Integer) return Integer is (x + y + z);
   function F3B (x : Integer; y : Integer; z : Integer) return Integer is (x + y + z);

   function F3Z (x, y, z : Integer) return Integer;
private
   function F3Z (x, y, z : Integer) return Integer is (x + y + z);
end Count_Subprogram;
