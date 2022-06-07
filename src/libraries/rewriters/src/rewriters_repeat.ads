with Libadalang.Analysis; use Libadalang.Analysis;
with Rewriters;           use Rewriters;

package Rewriters_Repeat is

   type Rewriter_Repeat is new Rewriter with private;

   overriding function Rewrite
     (R_R : Rewriter_Repeat; Unit : in out Analysis_Unit)
   return Boolean;

   function Get_Rewriter (R_R : Rewriter_Repeat) return Rewriter'Class;

   function Make_Rewriter_Repeat
     (R : Rewriter'Class) return Rewriter_Repeat;

private

   type Any_Rewriter is not null access Rewriter'Class;

   type Rewriter_Repeat is new Rewriter with record
      A_Rewriter : Any_Rewriter;
   end record;

   function Get_Rewriter (R_R : Rewriter_Repeat) return Rewriter'Class is
      (R_R.A_Rewriter.all);

   function Make_Rewriter_Repeat
     (R : Rewriter'Class) return Rewriter_Repeat is
     (A_Rewriter => new Rewriter'Class'(R));

end Rewriters_Repeat;
