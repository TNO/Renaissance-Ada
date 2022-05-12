with Ada.Containers;        use Ada.Containers;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Rejuvenation;          use Rejuvenation;
with Mark_Utils;            use Mark_Utils;

package body Match_Accepters_Mark_Aware is

   overriding function Is_Match_Acceptable
     (M_A : Match_Accepter_Mark_Aware; M_P : Match_Pattern) return Boolean
   is
      function Is_Marked_Match (Match : Match_Pattern) return Boolean;
      function Is_Marked_Match (Match : Match_Pattern) return Boolean is
         Match_Nodes : constant Node_List.Vector := Match.Get_Nodes;
         Match_Node  : constant Ada_Node         :=
           (if Match_Nodes.Length = 1 then Match_Nodes.First_Element
            else Match_Nodes.First_Element.Parent);
      begin
         return Is_Some_Parent_Marked (Match_Node);
      end Is_Marked_Match;
   begin
      return
        Is_Marked_Match (M_P)
        and then M_A.A_Match_Accepter.Is_Match_Acceptable (M_P);
   end Is_Match_Acceptable;

      function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace)
      return Rewriter_Find_And_Replace;
   function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace)
      return Rewriter_Find_And_Replace
   is
      function Is_Marked_Match (Match : Match_Pattern) return Boolean;
      function Is_Marked_Match (Match : Match_Pattern) return Boolean is
         Match_Nodes : constant Node_List.Vector := Match.Get_Nodes;
         Match_Node  : constant Ada_Node         :=
           (if Match_Nodes.Length = 1 then Match_Nodes.First_Element
            else Match_Nodes.First_Element.Parent);
      begin
         return Is_Some_Parent_Marked (Match_Node);
      end Is_Marked_Match;

      function Accept_Marked_Match (Match : Match_Pattern) return Boolean;
      function Accept_Marked_Match (Match : Match_Pattern) return Boolean is
         Return_Value : Boolean;
      begin
         Put_Line ("Accept_Marked_Match In");
         Return_Value := Is_Marked_Match (Match)
           and then R.Get_Match_Accepter.Is_Match_Acceptable (Match);
         Put_Line ("Accept_Marked_Match Out");
         return Return_Value;
      end Accept_Marked_Match;
   begin
      return Make_Rewriter_Find_And_Replace
        (R.Get_Find_Pattern, R.Get_Replace_Pattern,
         Make_Match_Accepter_Function_Access
           (Accept_Marked_Match'Unrestricted_Access));
   end Make_Rewriter_Find_And_Replace_Mark_Aware;

   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat)
      return Rewriter_Repeat;
   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat)
      return Rewriter_Repeat is
   begin
      return Make_Rewriter_Repeat (Make_Rewriter_Mark_Aware (R.Get_Rewriter));
   end Make_Rewriter_Repeat_Mark_Aware;

   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence)
      return Rewriter_Sequence;
   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence)
      return Rewriter_Sequence is
      Return_Value : Rewriters_Vectors.Vector;
   begin
      for Rewriter of R.Get_Vector loop
         Return_Value.Append (Make_Rewriter_Mark_Aware (Rewriter));
      end loop;
      return Make_Rewriter_Sequence (Return_Value);
   end Make_Rewriter_Sequence_Mark_Aware;

   function Make_Rewriter_Mark_Aware
     (R : Rewriter'Class) return Rewriter'Class is
   begin
      if R in Rewriter_Find_And_Replace'Class then
         return Make_Rewriter_Find_And_Replace_Mark_Aware
           (Rewriter_Find_And_Replace (R));
      elsif R in Rewriter_Repeat'Class then
         return Make_Rewriter_Repeat_Mark_Aware (Rewriter_Repeat (R));
      elsif R in Rewriter_Sequence'Class then
         return Make_Rewriter_Sequence_Mark_Aware (Rewriter_Sequence (R));
      else
         Assert (False, "Unknown Rewriter class");
         return R;
      end if;
   end Make_Rewriter_Mark_Aware;

end Match_Accepters_Mark_Aware;
