with Libadalang.Analysis; use Libadalang.Analysis;

package Shared is

   function Nr_Of_Parameters (S_S : Subp_Spec) return Integer;

   function Is_Part_Of_Subp_Def (S_S : Subp_Spec) return Boolean;

   function Inside_Private_Part (Node : Ada_Node'Class) return Boolean;

end Shared;
