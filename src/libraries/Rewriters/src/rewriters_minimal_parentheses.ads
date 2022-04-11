with Libadalang.Analysis;     use Libadalang.Analysis;
with Rejuvenation.Navigation; use Rejuvenation.Navigation;
with Rewriters;               use Rewriters;

package Rewriters_Minimal_Parentheses is

   type Rewriter_Minimal_Parentheses is new Rewriter with private;

   overriding function Rewrite
     (RMP       : Rewriter_Minimal_Parentheses; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String;

   overriding function Rewrite_Context
     (RMP : Rewriter_Minimal_Parentheses; Node : Ada_Node'Class)
      return Ada_Node with
      Post => Is_Reflexive_Ancestor (Rewrite_Context'Result, Node);

   function Make_Rewriter_Minimal_Parentheses
      return Rewriter_Minimal_Parentheses;

private

   type Rewriter_Minimal_Parentheses is new Rewriter with null record;

   function Make_Rewriter_Minimal_Parentheses
      return Rewriter_Minimal_Parentheses is
     (Rewriter with null record);

end Rewriters_Minimal_Parentheses;
