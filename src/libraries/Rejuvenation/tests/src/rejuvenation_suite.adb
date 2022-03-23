with AUnit.Simple_Test_Cases;          use AUnit.Simple_Test_Cases;
with Test_Find_And_Replacer;           use Test_Find_And_Replacer;
with Test_Indentation;                 use Test_Indentation;
with Test_Match_Patterns;              use Test_Match_Patterns;
with Test_Match_Patterns_Eagerness;    use Test_Match_Patterns_Eagerness;
with Test_Match_Patterns_Placeholders; use Test_Match_Patterns_Placeholders;
with Test_Navigation;                  use Test_Navigation;
with Test_Placeholders;                use Test_Placeholders;
with Test_Replacer;                    use Test_Replacer;
with Test_Nested;                      use Test_Nested;
with Test_String_Utils;                use Test_String_Utils;
with Test_Text_Rewrites;               use Test_Text_Rewrites;

package body Rejuvenation_Suite is

   function Suite return Access_Test_Suite is
      Ret               : constant Access_Test_Suite := new Test_Suite;
      Find_And_Replacer : constant Test_Case_Access  :=
        new Find_And_Replacer_Test_Case;
      Indentation    : constant Test_Case_Access := new Indentation_Test_Case;
      Match_Patterns : constant Test_Case_Access :=
        new Match_Patterns_Test_Case;
      Match_Patterns_Eagerness : constant Test_Case_Access :=
        new Match_Patterns_Eagerness_Test_Case;
      Match_Patterns_Placeholders : constant Test_Case_Access :=
        new Match_Patterns_Placeholders_Test_Case;
      Navigation   : constant Test_Case_Access := new Navigation_Test_Case;
      Placeholders : constant Test_Case_Access := new Placeholders_Test_Case;
      Replacer     : constant Test_Case_Access := new Replacer_Test_Case;
      Nested       : constant Test_Case_Access := new Nested_Test_Case;
      String_Utils : constant Test_Case_Access := new String_Utils_Test_Case;
      Rewrites     : constant Test_Case_Access := new Text_Rewrite_Test_Case;
   begin
      Ret.Add_Test (Find_And_Replacer);
      Ret.Add_Test (Indentation);
      Ret.Add_Test (Match_Patterns);
      Ret.Add_Test (Match_Patterns_Eagerness);
      Ret.Add_Test (Match_Patterns_Placeholders);
      Ret.Add_Test (Navigation);
      Ret.Add_Test (Placeholders);
      Ret.Add_Test (Replacer);
      Ret.Add_Test (Nested);
      Ret.Add_Test (String_Utils);
      Ret.Add_Test (Rewrites);
      return Ret;
   end Suite;

end Rejuvenation_Suite;
