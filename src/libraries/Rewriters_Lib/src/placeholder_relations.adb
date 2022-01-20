with Ada.Text_IO;         use Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Rejuvenation;        use Rejuvenation;
with Rejuvenation.Finder; use Rejuvenation.Finder;
with Rejuvenation.Utils;  use Rejuvenation.Utils;

package body Placeholder_Relations is

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
         when Ada_String_Literal | Ada_Int_Literal | Ada_Real_Literal =>
            return True;
         when Ada_Identifier =>
            return False;
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
              ("Is_Constant_Expression: Unhandled kind - " & E.Kind'Image);
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

end Placeholder_Relations;
