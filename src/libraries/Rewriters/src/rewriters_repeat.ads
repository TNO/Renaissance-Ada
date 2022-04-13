with Libadalang.Analysis;     use Libadalang.Analysis;
with Rejuvenation.Navigation; use Rejuvenation.Navigation;
with Rewriters;               use Rewriters;

package Rewriters_Repeat is

   type Rewriter_Repeat is new Rewriter with private;

   overriding function Rewrite
     (R_R       : Rewriter_Repeat; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String;

   overriding function Rewrite_Context
     (R_R : Rewriter_Repeat; Node : Ada_Node'Class) return Ada_Node with
      Post => Is_Reflexive_Ancestor (Rewrite_Context'Result, Node);

   function Make_Rewriter_Repeat (R : Rewriter) return Rewriter_Repeat;

private

   type Rewriter_Repeat is new Rewriter with record
      F_Rewriter : Rewriter;
   end record;

   function Make_Rewriter_Repeat (R : Rewriter) return Rewriter_Repeat is
      (Rewriter with R);

end Rewriters_Repeat;
