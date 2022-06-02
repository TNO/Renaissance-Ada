with Libadalang.Analysis;        use Libadalang.Analysis;
with Mark_Utils;                 use Mark_Utils;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Rewriters;                  use Rewriters;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Post_Processing_Contexts;   use Post_Processing_Contexts;
with Post_Processing_Contexts_Rewriter_Find_And_Replace;
use Post_Processing_Contexts_Rewriter_Find_And_Replace;

package Patchers is

   type Patcher is tagged private;

   function Name (P : Patcher) return String;

   function Prepare_Unit
     (P         :        Patcher;
      Unit      : in out Analysis_Unit)
   return Boolean;
   --  Prepare Unit for rewrite by
   --  1. Adding Pretty Print sections for line-based pretty printing
   --  2. Mark relevant nodes within the unit for post processing
   --  Return value signals whether any preparation was made.

   procedure Rewrite
     (P         :        Patcher;
      Unit      : in out Analysis_Unit);
   --  Rewrite Unit
   --  Rewrites should be limited to marked nodes (including their children)
   --
   --  We use unit (instead of filename) since it can also contain project info

   function Make_Patcher
     (N : String;
      C : Post_Processing_Context'Class;
      R : Rewriter'Class)
      return Patcher;

   function Make_Patcher
     (N : String;
      C : Post_Processing_Context'Class;
      R, P : Rewriter'Class)
      return Patcher;

   function Make_Patcher
     (N : String;
      R : Rewriter_Find_And_Replace)
      return Patcher;

   function Make_Patcher
     (N : String;
      R : Rewriter_Find_And_Replace;
      P : Rewriter'Class)
      return Patcher;

private

   type Any_Post_Processing_Context is access Post_Processing_Context'Class;

   type Any_Rewriter is access Rewriter'Class;

   type Patcher is tagged record
      F_Name         : Unbounded_String;
      A_Context      : Any_Post_Processing_Context;
      A_Rewriter     : Any_Rewriter;
      A_Post_Process : Any_Rewriter;
   end record;

   function Name (P : Patcher) return String
   is
     (To_String (P.F_Name));

   function Make_Patcher
     (N : String;
      C : Post_Processing_Context'Class;
      R : Rewriter'Class)
      return Patcher
   is
     (To_Unbounded_String (N), new Post_Processing_Context'Class'(C),
      new Rewriter'Class'(R), null);

   function Make_Patcher
     (N : String;
      C : Post_Processing_Context'Class;
      R, P : Rewriter'Class)
      return Patcher
   is
     (To_Unbounded_String (N), new Post_Processing_Context'Class'(C),
      new Rewriter'Class'(R),
      new Rewriter'Class'(Make_Rewriter_Mark_Aware (P)));

   function Make_Patcher
     (N : String;
      R : Rewriter_Find_And_Replace)
      return Patcher
   is
     (Make_Patcher
        (N,
         Make_Post_Processing_Context_Rewriter_Find_And_Replace (R),
         R));

   function Make_Patcher
     (N : String;
      R : Rewriter_Find_And_Replace;
      P : Rewriter'Class)
      return Patcher
   is
     (Make_Patcher
        (N,
         Make_Post_Processing_Context_Rewriter_Find_And_Replace (R),
         R,
         P));

end Patchers;
