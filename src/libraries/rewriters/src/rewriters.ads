with Libadalang.Analysis; use Libadalang.Analysis;

package Rewriters is

   type Rewriter is interface;

   function Rewrite
     (R : Rewriter; Unit : in out Analysis_Unit) return Boolean is abstract;
   --  Rewrite Unit
   --  Rewrites should be limited to marked nodes (including their children)
   --  Return value signals that Unit is changed
   --
   --  We use unit (instead of filename) since it can also contain project info

end Rewriters;
