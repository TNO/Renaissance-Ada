with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;

package body Rewriters_Minimal_Parentheses is

   overriding function Rewrite_Context
     (RMP : Rewriter_Minimal_Parentheses; Node : Ada_Node'Class)
      return Ada_Node
   is
   begin
      if Node.Is_Null
        or else Node.Kind not in Ada_Expr
        or else Node.Parent.Is_Null
      then
         return Node.As_Ada_Node;
      else
         return Node.Parent;
      end if;
   end Rewrite_Context;

   function Are_Brackets_Syntactically_Mandatory
     (ParenExpr : Paren_Expr) return Boolean;
   function Are_Brackets_Syntactically_Mandatory
     (ParenExpr : Paren_Expr) return Boolean
      --  for more info: see https://gt3-prod-2.adacore.com/#/tickets/U908-032

   is
      Parent : constant Ada_Node := ParenExpr.Parent;
   begin
      return
        Parent.Is_Null
        or else Parent.Kind in Ada_Expr_Function | Ada_Qual_Expr;
   end Are_Brackets_Syntactically_Mandatory;

   function Are_Brackets_Necessary (ParenExpr : Paren_Expr) return Boolean;
   --  Are Brackets necessary for this ParenExpr?

   function Are_Brackets_Necessary (ParenExpr : Paren_Expr) return Boolean
   --  A conservative implementation
   is
   begin
      case ParenExpr.F_Expr.Kind is
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
         --  'parenthesis directly within parenthesis'
         --  assume most inner-expression in parenthesis would require brackets
            declare
               Parent : constant Ada_Node := ParenExpr.Parent;
            begin
               return
                 Parent.Is_Null
                 or else Parent.Kind in Ada_Bin_Op | Ada_Relation_Op |
                     Ada_Un_Op | Ada_Membership_Expr;
            end;
         when others =>
            return False;
      end case;
   end Are_Brackets_Necessary;

   overriding function Rewrite
     (RMP       : Rewriter_Minimal_Parentheses; Node : Ada_Node'Class;
      Top_Level : Boolean := True) return String
   is
      TR : Text_Rewrite :=
        Make_Text_Rewrite_Node
          (Node, Trivia_On_Same_Line, Trivia_On_Same_Line);
   begin
      for PE_Node of Find (Node, Ada_Paren_Expr) loop
         declare
            ParenExpr : constant Paren_Expr := PE_Node.As_Paren_Expr;
         begin
            if not Are_Brackets_Syntactically_Mandatory (ParenExpr)
              and then not Are_Brackets_Necessary (ParenExpr)
            then
               TR.ReplaceAround
                 (Node      => ParenExpr, Before_Text => "",
                  Innernode => ParenExpr.F_Expr, After_Text => "",
                  Charset   => Node.Unit.Get_Charset);
            end if;
         end;
      end loop;
      return TR.ApplyToString;
   end Rewrite;

end Rewriters_Minimal_Parentheses;
