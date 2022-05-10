with Ada.Containers;              use Ada.Containers;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;

package body Rewriters_Find_And_Replace is

   function Matching_Nodes
     (RFR : Rewriter_Find_And_Replace; Unit : Analysis_Unit)
      return Node_List.Vector
   is
      Root    : constant Ada_Node                  := Unit.Root;
      Matches : constant Match_Pattern_List.Vector :=
        (if RFR.F_Find_Pattern.As_Ada_Node.Kind in Ada_Ada_List then
           Find_Sub_List (Root, RFR.F_Find_Pattern)
         else Find_Full (Root, RFR.F_Find_Pattern));
      Return_Value : Node_List.Vector;
   begin
      for Match of Matches loop
         if RFR.F_Match_Accepter (Match) then
            declare
               Match_Nodes : constant Node_List.Vector := Match.Get_Nodes;
               Match_Node  : constant Ada_Node         :=
                 (if Match_Nodes.Length = 1 then Match_Nodes.First_Element
                  else Match_Nodes.First_Element.Parent);
            begin
               Return_Value.Append (Match_Node);
            end;
         end if;
      end loop;
      return Return_Value;
   end Matching_Nodes;

   overriding procedure Rewrite
     (RFR : Rewriter_Find_And_Replace; Unit : in out Analysis_Unit)
   is
      T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      Find_And_Replace
        (T_R, Unit.Root, RFR.F_Find_Pattern, RFR.F_Replace_Pattern,
         RFR.F_Match_Accepter);
      T_R.Apply;
      Unit.Reparse;
   end Rewrite;

end Rewriters_Find_And_Replace;
