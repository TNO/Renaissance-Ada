with Ada.Assertions;                  use Ada.Assertions;
with Ada.Containers;                  use Ada.Containers;
with Ada.Text_IO;                     use Ada.Text_IO;
with Langkit_Support.Text;            use Langkit_Support.Text;
with Libadalang.Common;               use Libadalang.Common;
with Rejuvenation;                    use Rejuvenation;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Node_Locations;     use Rejuvenation.Node_Locations;
with Rejuvenation.Text_Rewrites;      use Rejuvenation.Text_Rewrites;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Repeat;                use Rewriters_Repeat;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;
with Mark_Utils;                      use Mark_Utils;

package body Rewriters_Mark_Aware is

   function Add_Mark_Awareness (R : Rewriter'Class) return Rewriter'Class;

   function Make_Rewriter_Mark_Aware
     (R : Rewriter'Class) return Rewriter_Mark_Aware
   is
      A_Rewriter : constant Any_Rewriter := new Rewriter'Class'(R);
   begin
      return
        (A_Original_Rewriter   => A_Rewriter,
         A_Mark_Aware_Rewriter =>
           new Rewriter'Class'(Add_Mark_Awareness (A_Rewriter.all)));
   end Make_Rewriter_Mark_Aware;

   function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace_Basic) return Rewriter_Find_And_Replace_Basic;
   function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace_Basic) return Rewriter_Find_And_Replace_Basic
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
         Return_Value :=
           Is_Marked_Match (Match)
           and then R.Get_Match_Accepter.Is_Match_Acceptable (Match);
         Put_Line ("Accept_Marked_Match Out");
         return Return_Value;
      end Accept_Marked_Match;
   begin
      return
        Make_Rewriter_Find_And_Replace
          (R.Get_Find_Pattern, R.Get_Replace_Pattern,
           Make_Match_Accepter_Function_Access
             (Accept_Marked_Match'Unrestricted_Access));
   end Make_Rewriter_Find_And_Replace_Mark_Aware;

   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat) return Rewriter_Repeat;
   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat) return Rewriter_Repeat
   is
   begin
      return Make_Rewriter_Repeat (Add_Mark_Awareness (R.Get_Rewriter));
   end Make_Rewriter_Repeat_Mark_Aware;

   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence) return Rewriter_Sequence;
   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence) return Rewriter_Sequence
   is
      Return_Value : Rewriters_Vectors.Vector;
   begin
      for Rewriter of R.Get_Vector loop
         Return_Value.Append (Add_Mark_Awareness (Rewriter));
      end loop;
      return Make_Rewriter_Sequence (Return_Value);
   end Make_Rewriter_Sequence_Mark_Aware;

   function Add_Mark_Awareness (R : Rewriter'Class) return Rewriter'Class is
   begin
      if R in Rewriter_Find_And_Replace_Basic'Class then
         return
           Make_Rewriter_Find_And_Replace_Mark_Aware
             (Rewriter_Find_And_Replace_Basic (R));
      elsif R in Rewriter_Repeat'Class then
         return Make_Rewriter_Repeat_Mark_Aware (Rewriter_Repeat (R));
      elsif R in Rewriter_Sequence'Class then
         return Make_Rewriter_Sequence_Mark_Aware (Rewriter_Sequence (R));
      else
         Assert (False, "Unknown Rewriter class");
         return R;
      end if;
   end Add_Mark_Awareness;

end Rewriters_Mark_Aware;
