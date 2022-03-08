pragma Assertion_Policy (Check);

with Libadalang.Analysis;
with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Direct_Calls is

   use type LALCO.Ada_Node_Kind_Type;

   function Is_Duplicate_Callsite (Name : LAL.Name) return Boolean;
   function Is_Duplicate_Callsite (Name : LAL.Name) return Boolean is

      function Is_Node_Duplicating_Parent (Node : LAL.Name) return Boolean;
      function Is_Node_Duplicating_Parent (Node : LAL.Name) return Boolean is
         Parent : constant LAL.Ada_Node'Class := Node.Parent;
      begin
         if Parent.Kind = LALCO.Ada_Call_Expr
           and then Node = Parent.As_Call_Expr.F_Name
         then
            declare
               Call_Expr : LAL.Call_Expr := Parent.As_Call_Expr;
            begin
               if Call_Expr.P_Called_Subp_Spec = LAL.No_Ada_Node
                 and then Call_Expr.Parent.Kind = LALCO.Ada_Call_Expr
                 and then
                   Call_Expr.Parent.As_Call_Expr.P_Called_Subp_Spec.Kind =
                   LALCO.Ada_Entry_Spec
               then
                  Call_Expr := Call_Expr.Parent.As_Call_Expr;
               end if;

               return Node.P_Called_Subp_Spec = Call_Expr.P_Called_Subp_Spec;
            end;
         else
            return
              Parent.Kind = LALCO.Ada_Dotted_Name
              and then Node = Parent.As_Dotted_Name.F_Suffix;
         end if;
      end Is_Node_Duplicating_Parent;

   begin
      if Name.Kind = LALCO.Ada_Dotted_Name
        or else Name.Kind in LALCO.Ada_Base_Id
      then
         return Is_Node_Duplicating_Parent (Name);
      else
         pragma Assert
           (Name.Kind = LALCO.Ada_Call_Expr, "Expected call expression");
         return False;
      end if;
   end Is_Duplicate_Callsite;

   function Is_Direct_Call (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Direct_Call (Node : LAL.Ada_Node'Class) return Boolean is
   begin
      case Node.Kind is
         when LALCO.Ada_Name =>
            return
              Node.As_Name.P_Is_Direct_Call
              and then not Is_Duplicate_Callsite (Node.As_Name);
         when LALCO.Ada_Un_Op_Range =>
            return
              not Utilities.Get_Referenced_Decl (Node.As_Un_Op.F_Op).Is_Null;
         when LALCO.Ada_Bin_Op_Range =>
            return
              not Utilities.Get_Referenced_Decl (Node.As_Bin_Op.F_Op).Is_Null;
         when others =>
            return False;
      end case;
   end Is_Direct_Call;

   function Get_Target (Expr : LAL.Expr'Class) return LAL.Basic_Decl;
   function Get_Target (Expr : LAL.Expr'Class) return LAL.Basic_Decl is
   begin
      case LALCO.Ada_Expr (Expr.Kind) is
         when LALCO.Ada_Name =>
            return
              Utilities.Get_Parent_Basic_Decl
                (Expr.As_Name.P_Called_Subp_Spec);
         when LALCO.Ada_Bin_Op_Range =>
            return Utilities.Get_Referenced_Decl (Expr.As_Bin_Op.F_Op);
         when LALCO.Ada_Un_Op_Range =>
            return Utilities.Get_Referenced_Decl (Expr.As_Un_Op.F_Op);
         when others =>
            raise Internal_Extraction_Error
              with "Cases in Is_Direct_Call and Get_Target do not match";
      end case;
   end Get_Target;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Is_Direct_Call (Node) then
         declare
            Expr   : constant LAL.Expr       := Node.As_Expr;
            Source : constant LAL.Basic_Decl :=
              Utilities.Get_Parent_Basic_Decl (Expr);
            Target     : constant LAL.Basic_Decl := Get_Target (Expr);
            Edge_Attrs : constant GW.Attribute_Value_Sets.Map :=
              Node_Edge_Types.Get_Edge_Attributes (Expr);
         begin
            Graph.Write_Edge
              (Source, Target, Node_Edge_Types.Edge_Type_Calls, Edge_Attrs);
         end;
      end if;
   end Extract_Edges;

end Extraction.Direct_Calls;
