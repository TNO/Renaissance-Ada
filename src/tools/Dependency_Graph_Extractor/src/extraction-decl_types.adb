with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Decl_Types is

   use type LALCO.Ada_Node_Kind_Type;

   function Get_Subp_Spec
     (Basic_Decl : LAL.Basic_Decl) return LAL.Base_Subp_Spec;
   function Get_Subp_Spec
     (Basic_Decl : LAL.Basic_Decl) return LAL.Base_Subp_Spec
   is
   begin
      if Basic_Decl.Kind = LALCO.Ada_Generic_Subp_Renaming_Decl then
         return
           Get_Subp_Spec
             (Utilities.Get_Referenced_Decl
                (Basic_Decl.As_Generic_Subp_Renaming_Decl.F_Renames));
      else
         return Basic_Decl.P_Subp_Spec_Or_Null (Follow_Generic => True);
      end if;
   end Get_Subp_Spec;

   function Get_Params (Subp_Spec : LAL.Base_Subp_Spec) return LAL.Params;
   function Get_Params (Subp_Spec : LAL.Base_Subp_Spec) return LAL.Params is
   begin
      case LALCO.Ada_Base_Subp_Spec (Subp_Spec.Kind) is
         when LALCO.Ada_Entry_Spec =>
            return Subp_Spec.As_Entry_Spec.F_Entry_Params;
         when LALCO.Ada_Subp_Spec =>
            return Subp_Spec.As_Subp_Spec.F_Subp_Params;
         when others =>
            raise Internal_Extraction_Error
              with "Unexpected Base_Subp_Spec in Get_Params";
      end case;
   end Get_Params;

   function Get_Return_Type_Expr
     (Subp_Spec : LAL.Base_Subp_Spec) return LAL.Type_Expr;
   function Get_Return_Type_Expr
     (Subp_Spec : LAL.Base_Subp_Spec) return LAL.Type_Expr
   is
   begin
      case LALCO.Ada_Base_Subp_Spec (Subp_Spec.Kind) is
         when LALCO.Ada_Entry_Spec =>
            return LAL.No_Type_Expr;
         when LALCO.Ada_Subp_Spec =>
            return Subp_Spec.As_Subp_Spec.F_Subp_Returns;
         when others =>
            raise Internal_Extraction_Error
              with "Unexpected Base_Subp_Spec in Get_Return_Type_Expr";
      end case;
   end Get_Return_Type_Expr;

   function Get_Type (Type_Def : LAL.Type_Def) return LAL.Basic_Decl;

   function Get_Type (Type_Expr : LAL.Type_Expr'Class) return LAL.Basic_Decl;
   function Get_Type (Type_Expr : LAL.Type_Expr'Class) return LAL.Basic_Decl is
   begin
      case LALCO.Ada_Type_Expr (Type_Expr.Kind) is
         when LALCO.Ada_Anonymous_Type =>
            return
              Get_Type (Type_Expr.As_Anonymous_Type.F_Type_Decl.F_Type_Def);
         when LALCO.Ada_Subtype_Indication =>
            return
              Utilities.Get_Referenced_Decl
                (Type_Expr.As_Subtype_Indication.F_Name);
         when others =>
            raise Internal_Extraction_Error
              with "Unexpected Type_Expr in Get_Type";
      end case;
   end Get_Type;

   function Get_Type (Type_Def : LAL.Type_Def) return LAL.Basic_Decl is
   begin
      case LALCO.Ada_Type_Def (Type_Def.Kind) is
         when LALCO.Ada_Array_Type_Def =>
            return
              Get_Type
                (Type_Def.As_Array_Type_Def.F_Component_Type.F_Type_Expr);
         when LALCO.Ada_Type_Access_Def =>
            return Get_Type (Type_Def.As_Type_Access_Def.F_Subtype_Indication);
         when LALCO.Ada_Access_To_Subp_Def =>
            return LAL.No_Basic_Decl;
         when others =>
            raise Internal_Extraction_Error
              with "Unexpected Type_Def in Get_Type";
      end case;
   end Get_Type;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node) then
         declare
            Source_Decl : constant LAL.Basic_Decl     := Node.As_Basic_Decl;
            Subp_Spec   : constant LAL.Base_Subp_Spec :=
              Get_Subp_Spec (Source_Decl);
         begin
            if Source_Decl.Kind /= LALCO.Ada_Enum_Literal_Decl
              and then not Subp_Spec.Is_Null
            then
               declare
                  Params      : constant LAL.Params := Get_Params (Subp_Spec);
                  Return_Type : constant LAL.Type_Expr :=
                    Get_Return_Type_Expr (Subp_Spec);
               begin
                  if not Params.Is_Null then
                     for Param_Spec of Params.F_Params loop
                        declare
                           Type_Expr : constant LAL.Type_Expr :=
                             Param_Spec.F_Type_Expr;
                           Target_Decl : constant LAL.Basic_Decl :=
                             Get_Type (Type_Expr);
                           Edge_Attrs : constant GW.Attribute_Value_Sets.Map :=
                             Node_Edge_Types.Get_Edge_Attributes
                               (Type_Expr, Can_Have_Array_Type => False);
                        begin
                           if not Target_Decl.Is_Null then
                              Graph.Write_Edge
                                (Source_Decl, Target_Decl,
                                 Node_Edge_Types
                                   .Edge_Type_Has_Parameter_Of_Type,
                                 Edge_Attrs);
                           end if;
                        end;
                     end loop;
                  end if;

                  if not Return_Type.Is_Null then
                     declare
                        Target_Decl : constant LAL.Basic_Decl :=
                          Get_Type (Return_Type);
                        Edge_Attrs : constant GW.Attribute_Value_Sets.Map :=
                          Node_Edge_Types.Get_Edge_Attributes
                            (Return_Type, Can_Have_Array_Type => False);
                     begin
                        if not Target_Decl.Is_Null then
                           Graph.Write_Edge
                             (Source_Decl, Target_Decl,
                              Node_Edge_Types.Edge_Type_Has_Return_Of_Type,
                              Edge_Attrs);
                        end if;
                     end;
                  end if;
               end;
            elsif Source_Decl.Kind in LALCO.Ada_Component_Decl |
                  LALCO.Ada_Discriminant_Spec | LALCO.Ada_Object_Decl
            then
               declare
                  Type_Expr : constant LAL.Type_Expr :=
                    Source_Decl.P_Type_Expression;
                  Target_Decl : constant LAL.Basic_Decl :=
                    Get_Type (Type_Expr);
                  Edge_Attrs : constant GW.Attribute_Value_Sets.Map :=
                    Node_Edge_Types.Get_Edge_Attributes
                      (Type_Expr, Can_Have_Array_Type => True);
               begin
                  if not Target_Decl.Is_Null then
                     for Source_Name of Source_Decl.P_Defining_Names loop
                        Graph.Write_Edge
                          (Source_Name, Source_Decl, Target_Decl,
                           Node_Edge_Types.Edge_Type_Has_Type, Edge_Attrs);
                     end loop;
                  end if;
               end;
            end if;
         end;
      end if;
   end Extract_Edges;

end Extraction.Decl_Types;
