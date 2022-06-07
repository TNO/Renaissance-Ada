with Libadalang.Analysis; use Libadalang.Analysis;
with Rejuvenation;        use Rejuvenation;

package Post_Processing_Contexts is

   type Post_Processing_Context is interface;

   function Get_Post_Processing_Context
     (P_P_C : Post_Processing_Context;
      Unit  : Analysis_Unit) return
     Node_List.Vector
   is abstract;
   --  Context (as indicated by the nodes)
   --  to which post processing should be limited.

end Post_Processing_Contexts;
