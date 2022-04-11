with Ada.Assertions;    use Ada.Assertions;
with Libadalang.Common; use Libadalang.Common;

package body Rewriters_Context_Utils is

   function Combine_Contexts (C1, C2 : Ada_Node) return Ada_Node
   is
   begin
      if Is_Reflexive_Ancestor (C1, C2)
      then
         return C1;
      else
         Assert (Check => Is_Reflexive_Ancestor (C2, C1),
                 Message => "Unexpectedly, contexts don't share same node.");
         return C2;
      end if;
   end Combine_Contexts;

   function To_Supported_Context (C : Ada_Node) return Ada_Node
   is
   begin
      --  workaround for https://gt3-prod-1.adacore.com/#/tickets/UB17-030
      --  solved in libadalang version 23.0 and higher
      case C.Kind is
         when Ada_While_Loop_Spec | Ada_Elsif_Stmt_Part_List =>
            return C.Parent;
         when Ada_Case_Stmt_Alternative | Ada_Elsif_Stmt_Part =>
            return C.Parent.Parent;
         when others =>
            return C;
      end case;
   end To_Supported_Context;

end Rewriters_Context_Utils;
