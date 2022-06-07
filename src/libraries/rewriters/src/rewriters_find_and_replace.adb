with Ada.Containers;                 use Ada.Containers;
with Libadalang.Common;              use Libadalang.Common;
with Rejuvenation.Finder;            use Rejuvenation.Finder;
with Rejuvenation.Find_And_Replacer; use Rejuvenation.Find_And_Replacer;
with Rejuvenation.Match_Patterns;    use Rejuvenation.Match_Patterns;
with Rejuvenation.Text_Rewrites;     use Rejuvenation.Text_Rewrites;
with Rejuvenation.Utils;             use Rejuvenation.Utils;

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
         if RFR.A_Match_Accepter.Is_Match_Acceptable (Match) then
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

   overriding function Rewrite
     (RFR : Rewriter_Find_And_Replace; Unit : in out Analysis_Unit)
      return Boolean
   is
      function Accept_Match (M_P : Match_Pattern) return Boolean;
      function Accept_Match (M_P : Match_Pattern) return Boolean is
        (RFR.A_Match_Accepter.Is_Match_Acceptable (M_P));

      T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      Find_And_Replace
        (T_R, Unit.Root, RFR.F_Find_Pattern, RFR.F_Replace_Pattern,
         Accept_Match'Access);
      if T_R.HasReplacements then
         T_R.Apply;
         Unit.Reparse;
         return True;
      else
         return False;
      end if;
   end Rewrite;

end Rewriters_Find_And_Replace;
