with Libadalang.Analysis; use Libadalang.Analysis;
with Rewriters;           use Rewriters;

package Rewriters_Repeat is

   type Rewriter_Repeat is new Rewriter with private;

   overriding procedure Rewrite
     (R_R : Rewriter_Repeat; Unit : in out Analysis_Unit);

   function Make_Rewriter_Repeat
     (R : Rewriter'Class) return Rewriter_Repeat;

private

   type Any_Rewriter is not null access Rewriter'Class;

   type Rewriter_Repeat is new Rewriter with record
      F_Rewriter : Any_Rewriter;
   end record;

   function Make_Rewriter_Repeat
     (R : Rewriter'Class) return Rewriter_Repeat is
     (F_Rewriter => new Rewriter'Class'(R));

end Rewriters_Repeat;
