with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Derived_Type_Defs is

   use type LALCO.Ada_Node_Kind_Type;

   function Has_Interfaces (Node : LAL.Ada_Node'Class) return Boolean is
     (Node.Kind in LALCO.Ada_Derived_Type_Def | LALCO.Ada_Task_Def |
          LALCO.Ada_Protected_Type_Decl | LALCO.Ada_Single_Protected_Decl);

   function Get_Interfaces (Node : LAL.Ada_Node'Class) return LAL.Parent_List;
   function Get_Interfaces (Node : LAL.Ada_Node'Class) return LAL.Parent_List
   is
   begin
      case Node.Kind is
         when LALCO.Ada_Derived_Type_Def =>
            return Node.As_Derived_Type_Def.F_Interfaces;
         when LALCO.Ada_Task_Def =>
            return Node.As_Task_Def.F_Interfaces;
         when LALCO.Ada_Protected_Type_Decl =>
            return Node.As_Protected_Type_Decl.F_Interfaces;
         when LALCO.Ada_Single_Protected_Decl =>
            return Node.As_Single_Protected_Decl.F_Interfaces;
         when others =>
            raise Internal_Extraction_Error
              with "Cases in Has_Interfaces and Get_Interfaces do not match";
      end case;
   end Get_Interfaces;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Node.Kind = LALCO.Ada_Derived_Type_Def then
         declare
            Derived_Type_Def : constant LAL.Derived_Type_Def :=
              Node.As_Derived_Type_Def;
            Type_Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Parent_Basic_Decl (Derived_Type_Def);
            Subtype_Indication : constant LAL.Subtype_Indication :=
              Derived_Type_Def.F_Subtype_Indication;
         begin
            if not Subtype_Indication.Is_Null then
               declare
                  Parent : constant LAL.Basic_Decl :=
                    Utilities.Get_Referenced_Decl (Subtype_Indication.F_Name);
               begin
                  Graph.Write_Edge
                    (Type_Decl, Parent,
                     Node_Edge_Types.Edge_Type_Derives_From);
               end;
            end if;
         end;
      end if;

      if Has_Interfaces (Node) then
         declare
            Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Parent_Basic_Decl (Node);
         begin
            for Interface_Ref of Get_Interfaces (Node) loop
               declare
                  Parent : constant LAL.Basic_Decl :=
                    Utilities.Get_Referenced_Decl (Interface_Ref);
               begin
                  Graph.Write_Edge
                    (Decl, Parent, Node_Edge_Types.Edge_Type_Derives_From);
               end;
            end loop;
         end;
      end if;
   end Extract_Edges;

end Extraction.Derived_Type_Defs;
