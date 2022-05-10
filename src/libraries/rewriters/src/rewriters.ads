with Libadalang.Analysis;         use Libadalang.Analysis;

package Rewriters is

   type Rewriter is interface;

   procedure Rewrite
     (R         :        Rewriter;
      Unit      : in out Analysis_Unit) is abstract;
   --  Rewrite Unit
   --  Rewrites should be limited to marked nodes (including their children)
   --
   --  We use unit (instead of filename) since it can also contain project info

end Rewriters;
