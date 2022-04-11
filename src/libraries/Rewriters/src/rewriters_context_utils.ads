with Libadalang.Analysis;      use Libadalang.Analysis;
with Rejuvenation.Navigation;  use Rejuvenation.Navigation;

package Rewriters_Context_Utils is

   function Combine_Contexts (C1, C2 : Ada_Node) return Ada_Node
     with Pre =>
       Is_Reflexive_Ancestor (C1, C2)
       or else Is_Reflexive_Ancestor (C2, C1),
     Post =>
       Is_Reflexive_Ancestor (Combine_Contexts'Result, C1)
       and then Is_Reflexive_Ancestor (Combine_Contexts'Result, C2);

   function To_Supported_Context (C : Ada_Node) return Ada_Node;
   --  Not all context can be parsed by LibAdaLang
end Rewriters_Context_Utils;
