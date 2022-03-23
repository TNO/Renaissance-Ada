with Rejuvenation.Finder;       use Rejuvenation.Finder;
with Rejuvenation.Placeholders; use Rejuvenation.Placeholders;
with Rejuvenation.Replacer;     use Rejuvenation.Replacer;
with String_Maps;               use String_Maps;

package body Rejuvenation.Find_And_Replacer is

   function Accept_All_Matches (Match : Match_Pattern) return Boolean is
      pragma Unreferenced (Match);
      --  In Ada 2022 replace with aspect on formal parameter Match
      --  + make function expression!
   begin
      return True;
   end Accept_All_Matches;

   procedure Find_And_Replace
     (TR : in out Text_Rewrite'Class; Node : Ada_Node'Class;
      Find_Pattern, Replace_Pattern :        Pattern;
      Accept_Match :        Match_Accepter := Accept_All_Matches'Access;
      Before, After                 :        Node_Location  := No_Trivia)
   is
      function Get_Placeholder_Replacement (Match : Match_Pattern) return Map;
      function Get_Placeholder_Replacement (Match : Match_Pattern) return Map
      is
         Result : Map;
      begin
         for Placeholder_Name of Rejuvenation.Placeholders
           .Get_Placeholder_Names
           (Replace_Pattern.As_Ada_Node)
         loop
            declare
               Placeholder_Nodes : constant Node_List.Vector :=
                 Match.Get_Placeholder_As_Nodes (Placeholder_Name);
            begin
               if Placeholder_Nodes.Length = 0 then
                  Result.Insert (Placeholder_Name, "");
               else
                  declare
                     TN : Text_Rewrite :=
                       Make_Text_Rewrite_Nodes
                         (Placeholder_Nodes.First_Element,
                          Placeholder_Nodes.Last_Element, All_Trivia,
                          All_Trivia);
                     --  always include comments of place holders
                  begin
                     for Node of Placeholder_Nodes loop
                        Find_And_Replace
                          (TN, Node, Find_Pattern, Replace_Pattern,
                           Accept_Match, Before, After);
                     end loop;
                     Result.Insert (Placeholder_Name, TN.ApplyToString);
                  end;
               end if;
            end;
         end loop;
         return Result;
      end Get_Placeholder_Replacement;

      Matches : constant Match_Pattern_List.Vector :=
        (if Find_Pattern.As_Ada_Node.Kind in Ada_Ada_List then
           Find_Non_Contained_Sub_List (Node, Find_Pattern)
         else Find_Non_Contained_Full (Node, Find_Pattern));
   begin
      for Match of Matches loop
         if Accept_Match (Match) then
            declare
               Match_Nodes  : constant Node_List.Vector := Match.Get_Nodes;
               Replacements : constant Map              :=
                 Get_Placeholder_Replacement (Match);
            begin
               TR.Replace
                 (Match_Nodes.First_Element, Match_Nodes.Last_Element,
                  Replace (Replace_Pattern.As_Ada_Node, Replacements), Before,
                  After, Node.Unit.Get_Charset);
            end;
         end if;
      end loop;
   end Find_And_Replace;

   function Find_And_Replace
     (File_Path     : String; Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match  : Match_Accepter := Accept_All_Matches'Access;
      Before, After : Node_Location  := No_Trivia) return Boolean
   is
      Ctx       : constant Analysis_Context := Create_Context;
      File_Unit : constant Analysis_Unit    := Ctx.Get_From_File (File_Path);
      TR        : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (File_Unit);
   begin
      Find_And_Replace
        (TR, File_Unit.Root, Find_Pattern, Replace_Pattern, Accept_Match,
         Before, After);
      TR.Apply;
      return TR.HasReplacements;
   end Find_And_Replace;

end Rejuvenation.Find_And_Replacer;
