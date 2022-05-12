with Libadalang.Analysis;   use Libadalang.Analysis;

package Patchers is

   type Patcher is interface;

   function Name (P : Patcher) return String is abstract;

   procedure Mark
     (P         :        Patcher;
      Unit      : in out Analysis_Unit) is abstract;
   --  Mark relevant nodes within the unit

   procedure Rewrite
     (P         :        Patcher;
      Unit      : in out Analysis_Unit) is abstract;
   --  Rewrite Unit
   --  Rewrites should be limited to marked nodes (including their children)
   --
   --  We use unit (instead of filename) since it can also contain project info

end Patchers;
