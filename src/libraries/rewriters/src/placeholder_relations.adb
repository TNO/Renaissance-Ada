with Ada.Assertions;            use Ada.Assertions;
with Ada.Containers;            use Ada.Containers;
with Ada.Text_IO;               use Ada.Text_IO;
with Langkit_Support.Text;      use Langkit_Support.Text;
with Libadalang.Common;         use Libadalang.Common;
with Rejuvenation;              use Rejuvenation;
with Rejuvenation.Finder;       use Rejuvenation.Finder;
with Rejuvenation.String_Utils; use Rejuvenation.String_Utils;
with Rejuvenation.Utils;        use Rejuvenation.Utils;

package body Placeholder_Relations is

   function Get_Expression_Type
     (Match : Match_Pattern;
      Expression : String)
      return Base_Type_Decl
   is
      E : constant Expr := Match.Get_Single_As_Node (Expression).As_Expr;
   begin
      return E.P_Expression_Type;
   end Get_Expression_Type;

   function Is_Referenced_In
     (D_N : Defining_Name; Node : Ada_Node) return Boolean;
   function Is_Referenced_In
     (D_N : Defining_Name; Node : Ada_Node) return Boolean
   is
      Identifiers : constant Node_List.Vector := Find (Node, Ada_Identifier);
   begin
      return
        (for some Identifier of Identifiers =>
           Identifier.As_Identifier.P_Referenced_Defining_Name = D_N);
   end Is_Referenced_In;

   function Is_Referenced_In
     (Match : Match_Pattern; Definition, Context : String) return Boolean
   is
      D_N : constant Defining_Name :=
        Match.Get_Single_As_Node (Definition).As_Defining_Name;
      Context_Nodes : constant Node_List.Vector :=
        Match.Get_Placeholder_As_Nodes (Context);
   begin
      return
        (for some Context_Node of Context_Nodes =>
           Is_Referenced_In (D_N, Context_Node));
   end Is_Referenced_In;

   function Is_Constant_Expression (E : Expr) return Boolean;
   function Is_Constant_Expression (E : Expr) return Boolean is
   begin
      case E.Kind is
         when Ada_String_Literal
            | Ada_Char_Literal
            | Ada_Int_Literal
            | Ada_Real_Literal
            | Ada_Null_Literal =>
            return True;
         when Ada_Identifier =>
            return False;
         when Ada_Un_Op =>
            declare
               U_O : constant Un_Op := E.As_Un_Op;
            begin
               return Is_Constant_Expression (U_O.F_Expr);
            end;
         when Ada_Bin_Op =>
            declare
               B_O : constant Bin_Op := E.As_Bin_Op;
            begin
               return
                 Is_Constant_Expression (B_O.F_Left)
                 and then Is_Constant_Expression (B_O.F_Right);
            end;
         when Ada_Relation_Op =>
            declare
               R_O : constant Relation_Op := E.As_Relation_Op;
            begin
               return
                 Is_Constant_Expression (R_O.F_Left)
                 and then Is_Constant_Expression (R_O.F_Right);
            end;
         when Ada_Paren_Expr =>
            return Is_Constant_Expression (E.As_Paren_Expr.F_Expr);
         when others =>
            Put_Line
              (Image (E.Full_Sloc_Image) &
               "Is_Constant_Expression: Unhandled kind - " & E.Kind'Image);
            return False;
      end case;
   end Is_Constant_Expression;

   function Is_Constant_Expression
     (Match : Match_Pattern; Expression : String) return Boolean
   is
      E : constant Expr := Match.Get_Single_As_Node (Expression).As_Expr;
   begin
      return Is_Constant_Expression (E);
   end Is_Constant_Expression;

   function Has_Side_Effect (E : Expr'Class) return Boolean;
   function Has_Side_Effect (E : Expr'Class) return Boolean is
      --  conservative implementation, see details in code.
   begin
      case E.Kind is
         --  TODO: add Ada_Attribute_Ref when it is clear
         --  whether users can define their own attribute function (in Ada2022)
         when Ada_String_Literal
            | Ada_Char_Literal
            | Ada_Int_Literal
            | Ada_Real_Literal
            | Ada_Null_Literal =>
            return False;
         when Ada_Identifier | Ada_Dotted_Name =>
            declare
               N : constant Libadalang.Analysis.Name := E.As_Name;
            begin
               --  conservative assumption: a function call has a side effect.
               return N.P_Is_Call;
            end;
         when Ada_Attribute_Ref =>
            --  conservative assumption:
            --  In Ada 2022, using Put_Image a user defined function
            --  with a possible side effect can be defined
            --  for the 'Image attribute
            return True;
         when Ada_Allocator =>
            --  TODO: find out whether allocator can have side effects!
            --  F_Subpool
            --  F_Type_Or_Expr
            return True;
         when Ada_Box_Expr =>
            --  Can occur in aggregates:
            --  The meaning is that the component of the aggregate takes
            --  the default value if there is one.
            return False;
         when Ada_If_Expr =>
            declare
               I_E : constant If_Expr := E.As_If_Expr;
            begin
               return Has_Side_Effect (I_E.F_Cond_Expr) or else
                 Has_Side_Effect (I_E.F_Then_Expr) or else
                 Has_Side_Effect (I_E.F_Else_Expr);
            end;
         when Ada_Case_Expr =>
            declare
               C_E : constant Case_Expr := E.As_Case_Expr;
            begin
               return Has_Side_Effect (C_E.F_Expr)
                 or else (for some C of C_E.F_Cases.Children =>
                            Has_Side_Effect (C.As_Expr));
            end;
         when Ada_Case_Expr_Alternative =>
            declare
               C_E_A : constant Case_Expr_Alternative :=
                 E.As_Case_Expr_Alternative;
            begin
               return Has_Side_Effect (C_E_A.F_Expr)
                 or else (for some C of C_E_A.F_Choices.Children =>
                            Has_Side_Effect (C.As_Expr));
            end;
         when Ada_Call_Expr =>
            declare
               C_E : constant Call_Expr := E.As_Call_Expr;
            begin
               --  conservative assumption: a function call has a side effect.
               --  TODO: analyse function call (out and in/out arguments)
               --        analyse function to have side effect
               --              * change variable not local to function
               --              * write to file / screen
               if C_E.P_Is_Call then
                  return True;
               else
                  case C_E.F_Suffix.Kind is
                     when Ada_Assoc_List =>
                        --   array access
                        declare
                           A_L : constant Assoc_List :=
                             C_E.F_Suffix.As_Assoc_List;
                        begin
                           return
                             (for some A of A_L.Children =>
                                Has_Side_Effect (A.As_Param_Assoc.F_R_Expr));
                        end;
                     when Ada_Bin_Op =>
                        --   array slice
                        declare
                           B_O : constant Bin_Op :=
                             C_E.F_Suffix.As_Bin_Op;
                        begin
                           Assert (Check => B_O.F_Op.Kind = Ada_Op_Double_Dot,
                                   Message => "Has Side Effects - Suffix "
                                   & "- Unexpected operator "
                                   & B_O.F_Op.Kind'Image);
                           return Has_Side_Effect (B_O);
                        end;
                     when others =>
                        Assert (Check => False,
                                Message => "Has Side Effects - Suffix "
                                & "- Unexpected kind "
                                & C_E.F_Suffix.Kind'Image);
                        return True;
                  end case;
               end if;
            end;
         when Ada_Paren_Expr =>
            return Has_Side_Effect (E.As_Paren_Expr.F_Expr);
         when Ada_Un_Op =>
            declare
               U_O : constant Un_Op := E.As_Un_Op;
            begin
               return Has_Side_Effect (U_O.F_Expr);
            end;
         when Ada_Bin_Op =>
            declare
               B_O : constant Bin_Op := E.As_Bin_Op;
            begin
               return
                 Has_Side_Effect (B_O.F_Left)
                 or else Has_Side_Effect (B_O.F_Right);
            end;
         when Ada_Relation_Op =>
            declare
               R_O : constant Relation_Op := E.As_Relation_Op;
            begin
               return
                 Has_Side_Effect (R_O.F_Left)
                 or else Has_Side_Effect (R_O.F_Right);
            end;
         when Ada_Aggregate =>
            declare
               A : constant Aggregate := E.As_Aggregate;
            begin
               return (not A.F_Ancestor_Expr.Is_Null
                       and then Has_Side_Effect (A.F_Ancestor_Expr))
                 or else (for some Assoc of A.F_Assocs.Children =>
                            Has_Side_Effect
                              (Assoc.As_Aggregate_Assoc.F_R_Expr));
            end;
         when Ada_Membership_Expr =>
            declare
               M_E : constant Membership_Expr := E.As_Membership_Expr;
            begin
               return Has_Side_Effect (M_E.F_Expr)
                 or else
                   (for some Alternative of M_E.F_Membership_Exprs.Children =>
                      Has_Side_Effect (Alternative.As_Expr));
            end;
         when Ada_Explicit_Deref =>
            declare
               E_D : constant Explicit_Deref := E.As_Explicit_Deref;
            begin
               return Has_Side_Effect (E_D.F_Prefix);
            end;
         when others =>
            Put_Line
              (Image (E.Full_Sloc_Image) &
                 " - Has_Side_Effect: Unhandled kind - " & E.Kind'Image);
            --  conservative assumption: unknown kind has a side effect.
            return True;
      end case;
   end Has_Side_Effect;

   function Has_Side_Effect
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean
   is
      --  Basic implementation:
      --  statement and declarations always have side effects
      --  e.g. change variable and introduce definition
      Nodes : constant Node_List.Vector :=
        Match.Get_Placeholder_As_Nodes (Placeholder_Name);
   begin
      return (for some Node of Nodes =>
                Node.Kind not in Ada_Expr
              or else Has_Side_Effect (Node.As_Expr));
   end Has_Side_Effect;

   function Has_Effect_On (A, B : Ada_Node) return Boolean;
   pragma Extensions_Allowed (On);
   function Has_Effect_On (A : Ada_Node;
                           B : Ada_Node with Unreferenced)
   return Boolean
   is
      --  Basic implementation:
      --  When an expression has no side effects,
      --  it has no effect on B
      --
      --  All Nodes A that effect Node B are reported as True.
      --  Yet, not all nodes A that do not effect node B are reported as False.
      --
      --  TODO: use the variables that are written by A and
      --        read by B to make it more accurate.
      --
      --        Note: dependent effects include
      --       * output parameter of a function
      --         used in the other AST Node
      --       * side effect of a function (i.e. state change)
      --         used in the other AST Node
   begin
      return A.Kind not in Ada_Expr
        or else Has_Side_Effect (A.As_Expr);
   end Has_Effect_On;
   pragma Extensions_Allowed (Off);

   function Has_Effect_On
     (Match : Match_Pattern; Placeholder_A, Placeholder_B : String)
      return Boolean
   is
      Nodes_A : constant Node_List.Vector :=
        Match.Get_Placeholder_As_Nodes (Placeholder_A);
      Nodes_B : constant Node_List.Vector :=
        Match.Get_Placeholder_As_Nodes (Placeholder_B);
   begin
      return (for some Node_A of Nodes_A =>
                (for some Node_B of Nodes_B =>
                   Has_Effect_On (Node_A, Node_B)));
   end Has_Effect_On;

   function Are_Independent
     (Match : Match_Pattern; Placeholder_1, Placeholder_2 : String)
      return Boolean
   is
   begin
      return not Has_Effect_On (Match, Placeholder_1, Placeholder_2)
        and then not Has_Effect_On (Match, Placeholder_2, Placeholder_1);
   end Are_Independent;

   function Is_Within_Base_Subp_Body
     (Match : Match_Pattern; Subp_Name : String) return Boolean
   is
      Nodes : constant Node_List.Vector := Get_Nodes (Match);
   begin
      --  Since Nodes are part of a sublist - checking a single node is enough
      return
        (for some Parent of Nodes.First_Element.Parents =>
           Parent.Kind in Ada_Base_Subp_Body
         and then Subp_Name =
           Raw_Signature (Parent.As_Base_Subp_Body.F_Subp_Spec.F_Subp_Name));
   end Is_Within_Base_Subp_Body;

   function Is_Negation_Expression
     (E : Expr) return Boolean;
   function Is_Negation_Expression
     (E : Expr) return Boolean
   is
   begin
      if E.Is_Null then
         return False;
      else
         case E.Kind is
            when Ada_Paren_Expr =>
               declare
                  P_E : constant Paren_Expr := E.As_Paren_Expr;
               begin
                  return Is_Negation_Expression (P_E.F_Expr);
               end;
            when Ada_Un_Op =>
               declare
                  U_O : constant Un_Op := E.As_Un_Op;
               begin
                  return U_O.F_Op.Kind = Ada_Op_Not;
               end;
            when Ada_Bin_Op =>
               declare
                  B_O : constant Bin_Op := E.As_Bin_Op;
               begin
                  return
                    B_O.F_Op.Kind in Ada_Op_Neq | Ada_Op_Not_In
                    or else
                      (B_O.F_Op.Kind in Ada_Op_And_Then | Ada_Op_Or_Else
                       and then Is_Negation_Expression (B_O.F_Left)
                       and then Is_Negation_Expression (B_O.F_Right));
               end;
            when Ada_Quantified_Expr =>
               declare
                  Q_E : constant Quantified_Expr := E.As_Quantified_Expr;
               begin
                  return Is_Negation_Expression (Q_E.F_Expr);
               end;
            when others =>
               return False;
         end case;
      end if;
   end Is_Negation_Expression;

   function Is_Negation_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean
   is
      E : constant Expr := Match.Get_Single_As_Node (Placeholder_Name).As_Expr;
   begin
      return Is_Negation_Expression (E);
   end Is_Negation_Expression;

   --  Localize Ada Nodes in defining files

   Standard_Unit_Filename : constant String := "__standard";
   --  libadalang uses the standard unit for the standard type

   function Is_In_Standard_Unit (Node : Ada_Node'Class) return Boolean;
   function Is_In_Standard_Unit (Node : Ada_Node'Class) return Boolean
   is
   begin
      return Ends_With (Node.Unit.Get_Filename, Standard_Unit_Filename);
   end Is_In_Standard_Unit;

   function Is_In_AStrUnb (Node : Ada_Node'Class) return Boolean;
   function Is_In_AStrUnb (Node : Ada_Node'Class) return Boolean
   is
   begin
      return Ends_With (Node.Unit.Get_Filename, "\adainclude\a-strunb.ads");
   end Is_In_AStrUnb;

   --  Is Standard Ada Type checks

   function Is_Standard_Type_Expression
     (T : Base_Type_Decl; Standard_Type_Name : String)
      return Boolean;
   function Is_Standard_Type_Expression
     (T : Base_Type_Decl; Standard_Type_Name : String)
      return Boolean
   is
   begin
      Assert (Check => not T.Is_Null,
              Message => "Is_Standard_Type_Expression - "
              & "Unexpectedly Base Type Decl is null");
      return
        Raw_Signature (T.F_Name) = Standard_Type_Name
        and then Is_In_Standard_Unit (T);
   end Is_Standard_Type_Expression;

   function Is_Boolean_Expression
     (Match : Match_Pattern) return Boolean
   is
      Return_Value : Boolean := False;
   begin
      declare
         Nodes : constant Node_List.Vector := Match.Get_Nodes;
      begin
         if Nodes.Length = 1 then
            declare
               E : constant Expr := Nodes.First_Element.As_Expr;
            begin
               Assert (Check => not E.Is_Null,
                       Message => "Is_Boolean_Expression - "
                       & "Unexpectedly Expr is null");
               --  Put_Line
               --    ("Before P_Expression_Type - "
               --     & Image (E.Full_Sloc_Image));
               declare
                  --  P_Expression_Type has a bug causing:
                  --  raised LANGKIT_SUPPORT.ERRORS.PROPERTY_ERROR :
                  --                                              stack overflow
                  B_T_D : constant Base_Type_Decl := E.P_Expression_Type;
               begin
                  --  Put_Line
                  --    ("After P_Expression_Type");
                  Return_Value :=
                    Is_Standard_Type_Expression (B_T_D, "Boolean");
               end;
            end;
         end if;
         return Return_Value;
      end;
   end Is_Boolean_Expression;

   function Is_Boolean_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression
        (Get_Expression_Type (Match, Placeholder_Name), "Boolean"));

   function Is_Integer_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression
        (Get_Expression_Type (Match, Placeholder_Name), "Integer"));

   function Is_Float_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression
        (Get_Expression_Type (Match, Placeholder_Name), "Float"));

   function Is_String_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression
        (Get_Expression_Type (Match, Placeholder_Name), "String"));

   function Is_Unbounded_String
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean
   is
      T : constant Base_Type_Decl :=
        Get_Expression_Type (Match, Placeholder_Name);
   begin
      return Raw_Signature (T.F_Name) = "Unbounded_String"
        and then Is_In_AStrUnb (T);
   end Is_Unbounded_String;

   function Is_Referenced_Decl_Defined_In_AStrUnb
     (N : Libadalang.Analysis.Name)
      return Boolean
   is
      Decl : constant Basic_Decl := N.P_Referenced_Decl;
   begin
      Assert (Check => not Decl.Is_Null,
              Message => "Is_Referenced_Decl_Defined_In_AStrUnb - "
              & "Unexpectedly Decl is null");
      return Is_In_AStrUnb (Decl);
   end Is_Referenced_Decl_Defined_In_AStrUnb;

end Placeholder_Relations;
