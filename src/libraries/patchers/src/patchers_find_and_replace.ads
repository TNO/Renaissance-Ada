with Libadalang.Analysis;        use Libadalang.Analysis;
with Mark_Utils;                 use Mark_Utils;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Patchers;                   use Patchers;
with Rewriters;                  use Rewriters;
with Rewriters_Find_And_Replace; use Rewriters_Find_And_Replace;
with Rewriters_Mark_Aware;       use Rewriters_Mark_Aware;

package Patchers_Find_And_Replace is

   type Patcher_Find_And_Replace is new Patcher with private;

   overriding function Name (P : Patcher_Find_And_Replace) return String;

   overriding procedure Mark
     (P         :        Patcher_Find_And_Replace;
      Unit      : in out Analysis_Unit);

   overriding procedure Rewrite
     (P         :        Patcher_Find_And_Replace;
      Unit      : in out Analysis_Unit);

   function Make_Patcher_Find_And_Replace
     (N : String;
      R : Rewriter_Find_And_Replace_Basic)
      return Patcher_Find_And_Replace;

   function Make_Patcher_Find_And_Replace
     (N : String;
      R : Rewriter_Find_And_Replace_Basic;
      P : Rewriter'Class)
      return Patcher_Find_And_Replace;

private

   type Any_Rewriter is access Rewriter'Class;

   type Patcher_Find_And_Replace is new Patcher with record
      F_Name         : Unbounded_String;
      F_Rewriter     : Rewriter_Find_And_Replace_Basic;
      F_Post_Process : Any_Rewriter;
   end record;

   overriding function Name (P : Patcher_Find_And_Replace) return String
   is
      (To_String (P.F_Name));

   function Make_Patcher_Find_And_Replace
     (N : String;
      R : Rewriter_Find_And_Replace_Basic)
      return Patcher_Find_And_Replace
   is
     (To_Unbounded_String (N), R, null);

   function Make_Patcher_Find_And_Replace
     (N : String;
      R : Rewriter_Find_And_Replace_Basic;
      P : Rewriter'Class)
      return Patcher_Find_And_Replace
   is
     (To_Unbounded_String (N),
      R, new Rewriter'Class'(Make_Rewriter_Mark_Aware (P)));

end Patchers_Find_And_Replace;
