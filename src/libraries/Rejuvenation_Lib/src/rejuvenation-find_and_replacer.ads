with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;

package Rejuvenation.Find_And_Replacer is

   type Match_Accepter is not null access function
     (Match : Match_Pattern) return Boolean;

   function Accept_All_Matches (Match : Match_Pattern) return Boolean;

   procedure Find_And_Replace
     (TR : in out Text_Rewrite'Class; Node : Ada_Node'Class;
      Find_Pattern, Replace_Pattern :        Pattern;
      Accept_Match :        Match_Accepter := Accept_All_Matches'Access;
      Before, After                 :        Node_Location  := No_Trivia);

   function Find_And_Replace
     (File_Path     : String; Find_Pattern, Replace_Pattern : Pattern;
      Accept_Match  : Match_Accepter := Accept_All_Matches'Access;
      Before, After : Node_Location  := No_Trivia) return Boolean;
   --  Find instances of the given pattern and
   --  for each accepted match replaces using the given replacement pattern.
   --  Placeholder names defined in the find pattern that occur
   --  in the replacement pattern are replaced
   --  by the matching string at the placeholder.
   --  Return value indicates whether any match was found and accepted
   --  Note: When a match is found and accepted, the file will be changed

end Rejuvenation.Find_And_Replacer;
