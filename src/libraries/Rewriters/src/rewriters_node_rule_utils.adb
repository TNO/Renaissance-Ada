package body Rewriters_Node_Rule_Utils is

   function Node_To_Rule (Node : Ada_Node'Class) return Grammar_Rule is
   begin
      case Node.Kind is
         when Ada_Stmt =>
   --  We allow the replacement of a single statement by multiple statements.
   --  So we have to expect not just a single but multiple statements.
            return Stmts_Rule;
         when Ada_Basic_Decl =>
--  We allow the replacement of a single declaration by multiple declarations.
--  So we have to expect not just a single but multiple declarations.
            return Basic_Decls_Rule;
         when Ada_Expr =>
            return Expr_Rule;

            --  other possibilities due to context enlargements
         when Ada_Stmt_List =>
            return Stmts_Rule;
         when Ada_Handled_Stmts =>
            return Handled_Stmts_Rule;
         when Ada_Param_Assoc =>
            return Param_Assoc_Rule;
         when Ada_Aggregate_Assoc =>
            return Aggregate_Assoc_Rule;
         when Ada_Pragma_Argument_Assoc =>
            return Pragma_Argument_Rule;
         when Ada_Ada_Node_List =>
            return Decl_Part_Rule;
         when others =>
            declare
               Node_To_Rule_Expection : exception;
            begin
               raise Node_To_Rule_Expection
                 with "Node_To_Rule: unhandled " & Node.Kind'Image;
               --  when we are convinced we have all kinds covered,
               --  we can remove the exception
               return Default_Grammar_Rule;
            end;
      end case;
   end Node_To_Rule;

end Rewriters_Node_Rule_Utils;
