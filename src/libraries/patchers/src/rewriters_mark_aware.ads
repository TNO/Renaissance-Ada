with Libadalang.Analysis;             use Libadalang.Analysis;
with Rewriters;                       use Rewriters;

package Rewriters_Mark_Aware is

   type Rewriter_Mark_Aware is new Rewriter with private;

   overriding function Rewrite
     (R_R : Rewriter_Mark_Aware; Unit : in out Analysis_Unit)
   return Boolean;

   function Make_Rewriter_Mark_Aware
     (R : Rewriter'Class) return Rewriter_Mark_Aware;

private

   type Any_Rewriter is not null access Rewriter'Class;

   type Rewriter_Mark_Aware is new Rewriter with record
      A_Original_Rewriter : Any_Rewriter;
      --  To ensure object is not garbage collected
      A_Mark_Aware_Rewriter : Any_Rewriter;
   end record;

   overriding function Rewrite
     (R_R : Rewriter_Mark_Aware; Unit : in out Analysis_Unit)
      return Boolean is
      (R_R.A_Mark_Aware_Rewriter.Rewrite (Unit));

end Rewriters_Mark_Aware;
