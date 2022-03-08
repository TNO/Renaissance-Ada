with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Generic_Instantiations is

   use type LALCO.Ada_Node_Kind_Type;

   function Is_Generic_Instantiation
     (Node : LAL.Ada_Node'Class) return Boolean is
     (Node.Kind in LALCO.Ada_Generic_Package_Instantiation |
          LALCO.Ada_Generic_Subp_Instantiation);

   function Get_Instantiated_Decl
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Basic_Decl;
   function Get_Instantiated_Decl
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Basic_Decl
   is
   begin
      case LALCO.Ada_Basic_Decl (Basic_Decl.Kind) is
         when LALCO.Ada_Generic_Package_Instantiation =>
            return
              Utilities.Get_Referenced_Decl
                (Basic_Decl.As_Generic_Package_Instantiation
                   .F_Generic_Pkg_Name);
         when LALCO.Ada_Generic_Subp_Instantiation =>
            return
              Utilities.Get_Referenced_Decl
                (Basic_Decl.As_Generic_Subp_Instantiation.F_Generic_Subp_Name);
         when others =>
            raise Internal_Extraction_Error
              with "Cases in Is_Generic_Instantiation "
              & "and Get_Instantiated_Decl do not match";
      end case;
   end Get_Instantiated_Decl;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Is_Generic_Instantiation (Node) then
         declare
            Instantiation     : constant LAL.Basic_Decl := Node.As_Basic_Decl;
            Instantiated_Decl : constant LAL.Basic_Decl :=
              Get_Instantiated_Decl (Instantiation);
         begin
            Graph.Write_Edge
              (Instantiation, Instantiated_Decl,
               Node_Edge_Types.Edge_Type_Instantiates);
         end;
      end if;
   end Extract_Edges;

end Extraction.Generic_Instantiations;
