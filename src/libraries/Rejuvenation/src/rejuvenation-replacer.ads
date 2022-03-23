with Rejuvenation.Placeholders; use Rejuvenation.Placeholders;

with String_Maps; use String_Maps;

package Rejuvenation.Replacer is

   function Replace
     (Node : Ada_Node'Class;
      Replacements : Map)
      return String
     with
       Pre => (for all Placeholder_Name of Get_Placeholder_Names (Node) =>
                 Replacements.Contains (Placeholder_Name));

end Rejuvenation.Replacer;
