with Libadalang.Analysis;      use Libadalang.Analysis;
with Post_Processing_Contexts; use Post_Processing_Contexts;
with Rejuvenation;             use Rejuvenation;

package Post_Processing_Contexts_Function_Access is

   type Access_Function_Get_Post_Processing_Context is
   not null access function (Unit : Analysis_Unit) return Node_List.Vector;

   type Post_Processing_Context_Function_Access is
     new Post_Processing_Context with private;

   overriding function Get_Post_Processing_Context
     (P_P_C : Post_Processing_Context_Function_Access;
      Unit  : Analysis_Unit) return Node_List.Vector;

   function Make_Post_Processing_Context_Function_Access
     (A_F : Access_Function_Get_Post_Processing_Context)
     return Post_Processing_Context_Function_Access;

private

   type Post_Processing_Context_Function_Access is
     new Post_Processing_Context with
      record
         F_Function : Access_Function_Get_Post_Processing_Context;
      end record;

   overriding function Get_Post_Processing_Context
     (P_P_C : Post_Processing_Context_Function_Access;
      Unit  : Analysis_Unit) return Node_List.Vector
   is
     (P_P_C.F_Function (Unit));

   function Make_Post_Processing_Context_Function_Access
     (A_F : Access_Function_Get_Post_Processing_Context)
      return Post_Processing_Context_Function_Access
   is
      (F_Function => A_F);

end Post_Processing_Contexts_Function_Access;
