with Ada.Assertions; use Ada.Assertions;

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

end Rewriters_Context_Utils;
