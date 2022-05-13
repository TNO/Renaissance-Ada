with Ada.Containers;      use Ada.Containers;
with Libadalang.Analysis; use Libadalang.Analysis;
with Rejuvenation;        use Rejuvenation;
with Mark_Utils;          use Mark_Utils;

package body Match_Accepters_Marked is

   overriding function Is_Match_Acceptable
     (M_A : Match_Accepter_Marked; M_P : Match_Pattern) return Boolean
   is
      Match_Nodes : constant Node_List.Vector := M_P.Get_Nodes;
      Match_Node  : constant Ada_Node         :=
        (if Match_Nodes.Length = 1 then Match_Nodes.First_Element
         else Match_Nodes.First_Element.Parent);
   begin
      return Is_Some_Parent_Marked (Match_Node);
   end Is_Match_Acceptable;

end Match_Accepters_Marked;
