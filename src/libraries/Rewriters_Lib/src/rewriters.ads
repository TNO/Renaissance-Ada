with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;

package Rewriters is

   type Rewriter is tagged private;
   --  To be usable in an Ada.Containers.Vectors,
   --  a Rewriter can't be abstract and can't be an interface

   function Rewrite
     (R         : Rewriter;
      Node      : Ada_Node'Class;
      Top_Level : Boolean := True)
      return String;

   function Rewrite_Context
     (R         : Rewriter;
      Node      : Ada_Node'Class)
      return Ada_Node
     with Post => Is_Reflexive_Ancestor (Rewrite_Context'Result, Node);

   type Any_Rewriter is access Rewriter'Class;
   type Any_Constant_Rewriter is access constant Rewriter'Class;

private

   type Rewriter is
      tagged null record;

   function Rewrite_Context
     (R         : Rewriter;
      Node      : Ada_Node'Class)
      return Ada_Node
   is
      (Node.As_Ada_Node);

end Rewriters;
