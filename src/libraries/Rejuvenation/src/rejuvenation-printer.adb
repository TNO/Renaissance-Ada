with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Rejuvenation.Utils;    use Rejuvenation.Utils;

package body Rejuvenation.Printer is

   -- Private -------
   procedure Print (Str : in out Unbounded_String; Node : Ada_Node'Class);

   procedure Print (Str : in out Unbounded_String; Node : Ada_Node'Class)
   is
   begin
      if Node.Is_Null then
         Append (Str, Node.Image);
      else
         case Node.Kind is
            when Ada_Alternatives_List
               | Ada_Assoc_List
               | Ada_Defining_Name_List =>
               declare
                  Children : constant Ada_Node_Array := Node.Children;
               begin
                  for Index in Children'Range loop
                     if Index /= Node.First_Child_Index then
                        Append (Str, ", ");
                     end if;
                     Print (Str, Children (Index));
                  end loop;
               end;

            when Ada_Aggregate =>
               Append (Str, "(");
               if not Node.As_Aggregate.F_Ancestor_Expr.Is_Null then
                  Print (Str, Node.As_Aggregate.F_Ancestor_Expr);
                  Append (Str, " => ");
               end if;
               Print (Str, Node.As_Aggregate.F_Assocs);
               Append (Str, ")");

            when Ada_Aggregate_Assoc =>
               if Node.As_Aggregate_Assoc.F_Designators.Children'Length /= 0
               then
                  Print (Str, Node.As_Aggregate_Assoc.F_Designators);
                  Append (Str, " => ");
               end if;
               Print (Str, Node.As_Aggregate_Assoc.F_R_Expr);

            when Ada_Assign_Stmt =>
               Print (Str, Node.As_Assign_Stmt.F_Dest);
               Append (Str, " := ");
               Print (Str, Node.As_Assign_Stmt.F_Expr);

            when Ada_Attribute_Ref =>
               Print (Str, Node.As_Attribute_Ref.F_Prefix);
               Append (Str, "'");
               Print (Str, Node.As_Attribute_Ref.F_Attribute);
               if not Node.As_Attribute_Ref.F_Args.Is_Null then
                  Append (Str, "(");
                  Print (Str, Node.As_Attribute_Ref.F_Args);
                  Append (Str, ")");
               end if;

            when Ada_Bin_Op =>
               Print (Str, Node.As_Bin_Op.F_Left);
               Append (Str, " ");
               Print (Str, Node.As_Bin_Op.F_Op);
               Append (Str, " ");
               Print (Str, Node.As_Bin_Op.F_Right);

            when Ada_Call_Expr =>
               Print (Str, Node.As_Call_Expr.F_Name);
               Append (Str, "(");
               Print (Str, Node.As_Call_Expr.F_Suffix);
               Append (Str, ")");

            when Ada_Char_Literal =>
               Append (Str, "'");
               Append
                 (Str, Raw_Signature (Node));
               Append (Str, "'");

            when Ada_Delay_Stmt =>
               Append (Str, "delay ");
               if Node.As_Delay_Stmt.F_Has_Until then
                  Append (Str, " until ");
               end if;
               Print (Str, Node.As_Delay_Stmt.F_Expr);

            when Ada_Dotted_Name =>
               Print (Str, Node.As_Dotted_Name.F_Prefix);
               Append (Str, ".");
               Print (Str, Node.As_Dotted_Name.F_Suffix);

            when Ada_Elsif_Expr_Part_List
               | Ada_Expr_Alternatives_List =>
               declare
                  Children : constant Ada_Node_Array := Node.Children;
               begin
                  for Index in Children'Range loop
                     Print (Str, Children (Index));
                  end loop;
               end;

            when Ada_Explicit_Deref =>
               Print (Str, Node.As_Explicit_Deref.F_Prefix);
               Append (Str, ".all");

            when Ada_Identifier =>
               Append (Str, Raw_Signature (Node));

            when Ada_Defining_Name =>
               Print (Str, Node.As_Defining_Name.F_Name);

            when Ada_For_Loop_Var_Decl =>
               Print (Str, Node.As_For_Loop_Var_Decl.F_Id);

            when Ada_If_Expr =>
               Append (Str, "if ");
               Print (Str, Node.As_If_Expr.F_Cond_Expr);
               Append (Str, " then ");
               Print (Str, Node.As_If_Expr.F_Then_Expr);
               Print (Str, Node.As_If_Expr.F_Alternatives);
               Append (Str, " else ");
               Print (Str, Node.As_If_Expr.F_Else_Expr);

            when Ada_Int_Literal =>
               Append
                 (Str, Raw_Signature (Node));

            when Ada_Membership_Expr =>
               Print (Str, Node.As_Membership_Expr.F_Expr);
               Append (Str, " ");
               Print (Str, Node.As_Membership_Expr.F_Op);
               Append (Str, " ");
               Print (Str, Node.As_Membership_Expr.F_Membership_Exprs);

            when Ada_Null_Literal =>
               Append (Str, "null");

            when Ada_Op_Abs =>
               Append (Str, "abs");

            when Ada_Op_And =>
               Append (Str, "and");

            when Ada_Op_And_Then =>
               Append (Str, "and then");

            when Ada_Op_Concat =>
               Append (Str, "&");

            when Ada_Op_Div =>
               Append (Str, "/");

            when Ada_Op_Double_Dot =>
               Append (Str, "..");

            when Ada_Op_Eq =>
               Append (Str, "=");

            when Ada_Op_Gt =>
               Append (Str, ">");

            when Ada_Op_Gte =>
               Append (Str, ">=");

            when Ada_Op_In =>
               Append (Str, "in");

            when Ada_Op_Lt =>
               Append (Str, "<");

            when Ada_Op_Lte =>
               Append (Str, "<=");

            when Ada_Op_Minus =>
               Append (Str, "-");

            when Ada_Op_Mod =>
               Append (Str, "mod");

            when Ada_Op_Mult =>
               Append (Str, "*");

            when Ada_Op_Neq =>
               Append (Str, "/=");

            when Ada_Op_Not =>
               Append (Str, "not");

            when Ada_Op_Not_In =>
               Append (Str, "not in");

            when Ada_Op_Or =>
               Append (Str, "or");

            when Ada_Op_Or_Else =>
               Append (Str, "or else");

            when Ada_Op_Plus =>
               Append (Str, "+");

            when Ada_Op_Pow =>
               Append (Str, "**");

            when Ada_Op_Rem =>
               Append (Str, "rem");

            when Ada_Op_Xor =>
               Append (Str, "xor");

            when Ada_Others_Designator =>
               Append (Str, "others");

            when Ada_Param_Assoc =>
               if not Node.As_Param_Assoc.F_Designator.Is_Null then
                  Print (Str, Node.As_Param_Assoc.F_Designator);
                  Append (Str, " => ");
               end if;
               Print (Str, Node.As_Param_Assoc.F_R_Expr);

            when Ada_Paren_Expr =>
               Append (Str, "(");
               Print (Str, Node.As_Paren_Expr.F_Expr);
               Append (Str, ")");

            when Ada_Qual_Expr =>
               Print (Str, Node.As_Qual_Expr.F_Prefix);
               Append (Str, "'");
               Print (Str, Node.As_Qual_Expr.F_Suffix);

            when Ada_Real_Literal =>
               Append
                 (Str, Raw_Signature (Node));

            when Ada_Relation_Op =>
               Print (Str, Node.As_Relation_Op.F_Left);
               Append (Str, " ");
               Print (Str, Node.As_Relation_Op.F_Op);
               Append (Str, " ");
               Print (Str, Node.As_Relation_Op.F_Right);

            when Ada_Return_Stmt =>
               Append (Str, "return ");
               Print (Str, Node.As_Return_Stmt.F_Return_Expr);

            when Ada_String_Literal =>
               Append
                 (Str, Raw_Signature (Node));

            when Ada_Un_Op =>
               Print (Str, Node.As_Un_Op.F_Op);
               Append (Str, " ");
               Print (Str, Node.As_Un_Op.F_Expr);

            when others =>
               Append (Str, Node.Image);
               Put_Line ("??? Rejuvenation.Printer: " & Node.Image);
         end case;
      end if;
   end Print;

   -- Public -------
   function Print (Node : Ada_Node'Class) return String is
      Str : Unbounded_String;
   begin
      Print (Str, Node);
      return To_String (Str);
   end Print;

end Rejuvenation.Printer;
