with Ada.Containers;              use Ada.Containers;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;                 use Ada.Text_IO;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Pretty_Print;   use Rejuvenation.Pretty_Print;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;
with Rewriters_Node_Rule_Utils;   use Rewriters_Node_Rule_Utils;
with Rewriters_Sequence_Utils;    use Rewriters_Sequence_Utils;

package body Rewriters_Find_And_Replace is

   function Make_Contexts
     (Matches   : Match_Pattern_List.Vector; Accept_Match : Match_Accepter)
      return Node_List.Vector;
   function Make_Contexts
     (Matches   : Match_Pattern_List.Vector; Accept_Match : Match_Accepter)
      return Node_List.Vector
   is
      Contexts : Node_List.Vector;
   begin
      for Match of Matches loop
         if Accept_Match (Match) then
            declare
               Match_Nodes : constant Node_List.Vector := Match.Get_Nodes;
               Match_Node  : constant Ada_Node         :=
                 (if Match_Nodes.Length = 1 then Match_Nodes.First_Element
                  else Match_Nodes.First_Element.Parent);
            begin
               if
                 (for all Context of Contexts =>
                    not Is_Reflexive_Ancestor (Context, Match_Node))
               then
                  declare
                     New_Contexts : Node_List.Vector :=
                       Node_List.To_Vector (Match_Node, 1);
                  begin
                     for Context of Contexts loop
                        if not Is_Ancestor (Match_Node, Context) then
                           New_Contexts.Append (Context);
                        end if;
                     end loop;
                     Node_List.Assign (Contexts, New_Contexts);
                  end;
               end if;
            end;
         end if;
      end loop;
      return Contexts;
   end Make_Contexts;

   overriding function Rewrite
     (RFR       : Rewriter_Find_And_Replace; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String
   is
      TN : Text_Rewrite :=
        Make_Text_Rewrite_Node
          (Node, Trivia_On_Same_Line, Trivia_On_Same_Line);
      Matches : constant Match_Pattern_List.Vector :=
        (if RFR.F_Find_Pattern.As_Ada_Node.Kind in Ada_Ada_List then
           Find_Non_Contained_Sub_List (Node, RFR.F_Find_Pattern)
         else Find_Non_Contained_Full (Node, RFR.F_Find_Pattern));
      Contexts : constant Node_List.Vector :=
        Make_Contexts (Matches, RFR.F_Match_Accepter);
   begin
      for Context of Contexts loop
         declare
            TR : Text_Rewrite := Make_Text_Rewrite_Node (Context);
         begin
            Find_And_Replace
              (TR, Context, RFR.F_Find_Pattern,
               RFR.F_Replace_Pattern, RFR.F_Match_Accepter);
            declare
               Rule : constant Grammar_Rule :=
                 Node_To_Rule (Context);
               Match_Unit : constant Analysis_Unit :=
                 Analyze_Fragment (TR.ApplyToString, Rule);
               Rewritten_Instance : constant String :=
                 Rewrite (RFR.F_Rewriters, Match_Unit.Root, False, Rule);
            begin
               TN.Replace (Context, Rewritten_Instance);

               if Top_Level then
                  Surround_Node_By_Pretty_Print_Section
                    (TN, Context);
               end if;
            end;
         exception
            when others =>
               Put_Line
                 (Image (Context.Full_Sloc_Image) &
                  "Error in Context");
               raise;
         end;
      end loop;
      return TN.ApplyToString;
   exception
      when Error : others =>
         Put_Line
           ("Error in Rewrite - Rewriter_Find_And_Replace " &
            Exception_Message (Error));
         raise;
   end Rewrite;

end Rewriters_Find_And_Replace;
