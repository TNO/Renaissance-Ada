with Libadalang.Analysis; use Libadalang.Analysis;
with Rewriters;           use Rewriters;
with Rewriters_Vectors;   use Rewriters_Vectors;

package Rewriters_Sequence is

   type Rewriter_Sequence is new Rewriter with private;

   overriding procedure Rewrite
     (R_S : Rewriter_Sequence; Unit : in out Analysis_Unit);

   function Make_Rewriter_Sequence (V : Rewriters_Vectors.Vector)
                                    return Rewriter_Sequence;

private

   type Rewriter_Sequence is new Rewriter with record
      F_Vector : Rewriters_Vectors.Vector;
   end record;

   function Make_Rewriter_Sequence (V : Rewriters_Vectors.Vector)
                                    return Rewriter_Sequence is
     (Rewriter with V);

end Rewriters_Sequence;
