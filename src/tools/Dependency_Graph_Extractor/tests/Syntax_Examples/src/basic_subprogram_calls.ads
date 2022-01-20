package Basic_Subprogram_Calls is

   function F1 return Integer;
   
   function F2(I : Integer) return Integer;
   
   function F3(I : Integer := 42) return Integer;
   
   function F4 return Integer is
     (42);
   
   function F5(I: Integer) return Integer is
     (I);
   
   function F6(I : Integer := 42) return Integer is
     (I);

   function F10 return Integer;

   function F11 return Integer is
     (F10);
   
   function F12 return Integer;
   
   procedure P1;
   
   procedure P2(I : Integer);
   
   procedure P3(I : Integer := F1);
   
   procedure P4;
   
   procedure P7 is null;
   
   procedure P8;
   
   procedure Test;
   
   package Nested is

      function Nested_F1 return Integer;
      
      procedure Nested_P1;

   end Nested;
      
   function F_Overload(I: Integer) return Integer;
   
   function F_Overload(B: Boolean) return Integer;
   
   function F_Overload(I: Integer) return Boolean;

   procedure P_Overload(I : Integer);

   procedure P_Overload(B : in out Boolean);

   -- - Calls in pre-conditions with multiple for all/for some (reported)
   -- - Call in Type_Invariant not picked up (reported)
   -- - C in Convention aspect picked up as call (reported)
   -- - Classwide type issue (reported)
   -- - Crash in print (reported)
   
   -- To report
   -- - Calls on access variables not detected as calls (TODO: awaiting answer to class wide type issue)
   -- - Link attribute use to attribute definition
   
   -- TODO: Calls to Standard
   -- TODO: Calls to system packages
      
   --                                       attr/param  call  impl  decl   ("call" includes calls to overloaded operators)
   -- Ada_Basic_Subp_Decl
   -- - Ada_Generic_Formal_Subp_Decl         -           -     -           (looking at children: Ada_Abstract_Formal_Subp_Decl, Ada_Concrete_Formal_Subp_Decl)
   -- - Ada_Abstract_Subp_Decl               x           x     -
   -- - Ada_Abstract_Formal_Subp_Decl        x           x     -
   -- - Ada_Concrete_Formal_Subp_Decl        x           x     -
   -- - Ada_Subp_Decl                        x           x     x
   -- - Ada_Entry_Decl (task)                c           x     x           ("implementations" are accept statements)
   -- - Ada_Entry_Decl (protected)           c           x     c
   -- - Ada_Enum_Literal_Decl                x           x     -
   -- - Ada_Generic_Subp_Internal            -           -     -           (looking at parent: Ada_Generic_Subp_Decl)
   -- Ada_Base_Subp_Body
   -- - Ada_Expr_Function                    x           x     -
   -- - Ada_Null_Subp_Decl                   x           x     -
   -- - Ada_Subp_Body                        x           x     -
   -- - Ada_Subp_Renaming_Decl               x           x     x
   -- Ada_Subp_Body_Stub_Range
   -- - Ada_Subp_Body_Stub                   x           x     x
   --
   -- - Ada_Entry_Body
   -- - Ada_Generic_Subp_Decl                x           -     x
   -- - Ada_Generic_Subp_Instantiation       x           x     x
   -- - Ada_Generic_Subp_Renaming_Decl       x           -     c
   -- - access subp variable decl            -           n     -

   -- x: done
   -- -: cannot occur
   -- c: crashes
   -- n: not detected
   
private

   function F10 return Integer is
     (42);
   
end Basic_Subprogram_Calls;
