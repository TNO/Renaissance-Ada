with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Assert_AST is

   function Assert_AST
     (AST : String; Rule : Grammar_Rule; Message : String)
      return Analysis_Unit;
   function Assert_AST
     (AST : String; Rule : Grammar_Rule; Message : String) return Analysis_Unit
   is
   begin
      return Analyze_Fragment (AST, Rule);
   exception
      when others =>
         Assert
           (Condition => False,
            Message   =>
              Message & ASCII.LF & "AST = " & AST & ASCII.LF & "Rule = " &
              Rule'Image);
         return No_Analysis_Unit;
   end Assert_AST;

   procedure Assert_Equal_AST
     (Expected_String, Actual_String : String; Rule : Grammar_Rule;
      Message                        : String)
   is
      Expected_AST : constant Analysis_Unit :=
        Assert_AST
          (Expected_String, Rule,
           "Assert_Equal_AST - Expected_String is not an AST");
      Actual_AST : constant Analysis_Unit :=
        Assert_AST
          (Actual_String, Rule,
           "Assert_Equal_AST - Actual_String is not an AST");

      MP    : Match_Pattern;
      Match : constant Boolean :=
        Match_Full (MP, Expected_AST.Root, Actual_AST.Root);
   begin
      Assert
        (Condition => Match,
         Message   =>
           Message & ASCII.LF & "Actual = " & Actual_String & ASCII.LF &
           "Expected = " & Expected_String & ASCII.LF);
      Assert
        (Condition => not MP.Get_Nodes.Is_Empty,
         Message   => "Match so nodes expected");
   end Assert_Equal_AST;

end Assert_AST;
