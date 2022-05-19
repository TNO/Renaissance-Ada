package body Predefined_Rewriters_Minimal_Parentheses is

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

end Predefined_Rewriters_Minimal_Parentheses;
