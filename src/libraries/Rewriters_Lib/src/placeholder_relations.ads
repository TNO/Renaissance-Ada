with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;

package Placeholder_Relations is

   function Is_Referenced_In
     (Match : Match_Pattern;
      Definition, Context : String)
      return Boolean;

   function Is_Constant_Expression
     (Match      : Match_Pattern;
      Expression : String)
      return Boolean;
   --  Is Expression in Match a constant expression?
   --  Note: 3, 3+4, "abc" & "def",  and 4*(5+7) are all constant expressions

   function Is_Within_Base_Subp_Body
     (Match     : Match_Pattern;
      Subp_Name : String)
      return Boolean;

end Placeholder_Relations;
