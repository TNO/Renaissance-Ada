with Ada.Text_IO;                   use Ada.Text_IO;
with Langkit_Support.Text;          use Langkit_Support.Text;
with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Rejuvenation.Patterns;         use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory;   use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;            use Rejuvenation.Utils;

with Finder;   use Finder;

procedure Rejuvenation_Find is

   Units : constant Analysis_Units.Vector :=
     Analyze_Project ("C:\bright\itecembed\Source\itec.gpr");

   Find_Pattern : constant Pattern :=
     --  Make_Pattern ("for $S_Var'$S_Attribute use $S_Expr;",
     --                Basic_Decls_Rule);

     Make_Pattern
       ("if $S_Cond then "
        & "  $S_f ($M_before, $M_Designator => $S_Value1, $M_after);"
        & "else "
        & "  $S_f ($M_before, $M_Designator => $S_Value2, $M_after);"
        & "end if;",
     If_Stmt_Rule);

     --  Make_Pattern
     --    ("for $S_I in $S_Range loop if $S_Cond then $S_Var := false; end if; end loop;",
     --     Loop_Stmt_Rule);

     --  Make_Pattern
     --    ("for $S_I in $S_Range loop if $S_Cond then $S_Var := true; end if; end loop;",
     --     Loop_Stmt_Rule);

     --  Make_Pattern
     --    ("if $S_Cond then $M_Stmts_True; $S_Tail; else $M_Stmts_False; $S_Tail; end if;",
     --     If_Stmt_Rule);

   Count : Natural := 0;
begin
    for Unit of Units loop
      for Match of Find (Unit.Root, Find_Pattern) loop
         Count := Count + 1;
         Put_Line
           (Image (Match.Get_Nodes.First_Element.Full_Sloc_Image) &
            Raw_Signature (Match.Get_Nodes.First_Element, Match.Get_Nodes.Last_Element));
      end loop;
   end loop;
   Put_Line ("# Matches = " & Count'Image);
end Rejuvenation_Find;
