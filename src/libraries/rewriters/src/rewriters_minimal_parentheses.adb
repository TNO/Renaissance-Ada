with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;

package body Rewriters_Minimal_Parentheses is

   function Are_Parentheses_Syntactically_Mandatory
     (P_E : Paren_Expr) return Boolean;
   function Are_Parentheses_Syntactically_Mandatory
     (P_E : Paren_Expr) return Boolean
      --  for more info: see https://gt3-prod-2.adacore.com/#/tickets/U908-032
   is
      Parent : constant Ada_Node := P_E.Parent;
   begin
      return
        Parent.Is_Null
        or else Parent.Kind in Ada_Expr_Function | Ada_Qual_Expr;
   end Are_Parentheses_Syntactically_Mandatory;

   function Are_Parenthesis_Semantically_Necessary
     (P_E : Paren_Expr) return Boolean;
   --  Are Brackets necessary for this ParenExpr?

   function Are_Parenthesis_Semantically_Necessary
     (P_E : Paren_Expr) return Boolean
   --  TODO: Make  implementation less conservative

   is
   begin
      case P_E.F_Expr.Kind is
         when Ada_If_Expr | Ada_Case_Expr | Ada_Quantified_Expr |
           Ada_Decl_Expr =>
            --  see http://www.ada-auth.org/standards/12rat/html/Rat12-3-2.html
            --  conservative: we don't remove when used in positional call
            --  with single parameter
            return True;
         when Ada_Bin_Op | Ada_Relation_Op | Ada_Un_Op | Ada_Membership_Expr |
           Ada_Paren_Expr =>
            --  conservative: we don't remove when used in left operand
            --                e.g. (a + b) + c <==> a + b + c
            --  conservative, yet local handling of special case
            --  'parentheses directly within parentheses'
            --  assume most inner-expression in parentheses
            --  would require parentheses
            declare
               Parent : constant Ada_Node := P_E.Parent;
            begin
               return
                 Parent.Is_Null
                 or else Parent.Kind in Ada_Bin_Op | Ada_Relation_Op |
                     Ada_Un_Op | Ada_Membership_Expr;
            end;
         when others =>
            return False;
      end case;
   end Are_Parenthesis_Semantically_Necessary;

   function Are_Parentheses_Necessary (P_E : Paren_Expr) return Boolean is
     (Are_Parentheses_Syntactically_Mandatory (P_E)
      or else Are_Parenthesis_Semantically_Necessary (P_E));

   function Matching_Nodes
     (RMP : Rewriter_Minimal_Parentheses; Unit : Analysis_Unit)
      return Node_List.Vector
   is
      Return_Value : Node_List.Vector;
   begin
      for PE_Node of Find (Unit.Root, Ada_Paren_Expr) loop
         declare
            ParenExpr : constant Paren_Expr := PE_Node.As_Paren_Expr;
         begin
            if not Are_Parentheses_Necessary (ParenExpr)
              and then RMP.F_Node_Accepter (PE_Node)
            then
               Return_Value.Append (PE_Node);
            end if;
         end;
      end loop;
      return Return_Value;
   end Matching_Nodes;

   overriding procedure Rewrite
     (RMP : Rewriter_Minimal_Parentheses; Unit : in out Analysis_Unit)
   is
      T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      for PE_Node of Find (Unit.Root, Ada_Paren_Expr) loop
         declare
            ParenExpr : constant Paren_Expr := PE_Node.As_Paren_Expr;
         begin
            if not Are_Parentheses_Necessary (ParenExpr) and then
              RMP.F_Node_Accepter (PE_Node)
            then
                  T_R.ReplaceAround
                    (Node      => ParenExpr, Before_Text => "",
                     Innernode => ParenExpr.F_Expr, After_Text => "",
                     Charset   => Unit.Get_Charset);
            end if;
         end;
      end loop;
      T_R.Apply;
      Unit.Reparse;
   end Rewrite;

end Rewriters_Minimal_Parentheses;
