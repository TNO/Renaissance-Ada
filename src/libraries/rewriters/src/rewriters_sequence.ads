with Libadalang.Analysis; use Libadalang.Analysis;
with Rewriters;           use Rewriters;
with Rewriters_Vectors;   use Rewriters_Vectors;

package Rewriters_Sequence is

   type Rewriter_Sequence is new Rewriter with private;

   overriding function Rewrite
     (R_S : Rewriter_Sequence; Unit : in out Analysis_Unit)
   return Boolean;

   function Get_Vector
     (R_S : Rewriter_Sequence)
      return Rewriters_Vectors.Vector;

   function Make_Rewriter_Sequence (V : Rewriters_Vectors.Vector)
                                    return Rewriter_Sequence;

private

   type Rewriter_Sequence is new Rewriter with record
      F_Vector : Rewriters_Vectors.Vector;
   end record;

   function Get_Vector
     (R_S : Rewriter_Sequence)
      return Rewriters_Vectors.Vector
   is
     (R_S.F_Vector);

   function Make_Rewriter_Sequence (V : Rewriters_Vectors.Vector)
                                    return Rewriter_Sequence is
     (Rewriter with V);

end Rewriters_Sequence;
