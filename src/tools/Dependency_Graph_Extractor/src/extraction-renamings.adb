with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Renamings is

   use type LALCO.Ada_Node_Kind_Type;

   function Is_Renaming (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Renaming (Node : LAL.Ada_Node'Class) return Boolean is
   begin
      return
        Node.Kind in LALCO.Ada_Package_Renaming_Decl |
            LALCO.Ada_Generic_Package_Renaming_Decl  |
            LALCO.Ada_Subp_Renaming_Decl | LALCO.Ada_Generic_Subp_Renaming_Decl
        or else
        (Node.Kind = LALCO.Ada_Exception_Decl
         and then not Node.As_Exception_Decl.F_Renames.Is_Null)
        or else
        (Node.Kind = LALCO.Ada_Object_Decl
         and then not Node.As_Object_Decl.F_Renaming_Clause.Is_Null);
   end Is_Renaming;

   function Get_Renamed_Name
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Name;
   function Get_Renamed_Name
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Name
   is
   begin
      case LALCO.Ada_Basic_Decl (Basic_Decl.Kind) is
         when LALCO.Ada_Package_Renaming_Decl =>
            return
              Basic_Decl.As_Package_Renaming_Decl.F_Renames.F_Renamed_Object;
         when LALCO.Ada_Generic_Package_Renaming_Decl =>
            return Basic_Decl.As_Generic_Package_Renaming_Decl.F_Renames;
         when LALCO.Ada_Subp_Renaming_Decl =>
            return Basic_Decl.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object;
         when LALCO.Ada_Generic_Subp_Renaming_Decl =>
            return Basic_Decl.As_Generic_Subp_Renaming_Decl.F_Renames;
         when LALCO.Ada_Exception_Decl =>
            return Basic_Decl.As_Exception_Decl.F_Renames.F_Renamed_Object;
         when LALCO.Ada_Object_Decl =>
            return
              Basic_Decl.As_Object_Decl.F_Renaming_Clause.F_Renamed_Object;
         when others =>
            raise Internal_Extraction_Error
              with "Cases in Is_Renaming and Get_Renamed_Name do not match";
      end case;
   end Get_Renamed_Name;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Utilities.Is_Relevant_Basic_Decl (Node) and then Is_Renaming (Node)
        and then not Utilities.Get_Referenced_Decl
          (Get_Renamed_Name (Node.As_Basic_Decl))
          .Is_Null -- Ignore builtins.
      then
         declare
            Renaming_Decl : constant LAL.Basic_Decl    := Node.As_Basic_Decl;
            Renamed_Name  : constant LAL.Defining_Name :=
              Utilities.Get_Referenced_Defining_Name
                (Get_Renamed_Name (Renaming_Decl));
            Renamed_Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Referenced_Decl (Get_Renamed_Name (Renaming_Decl));
         begin
            Graph.Write_Edge
              (Renaming_Decl, Renamed_Name, Renamed_Decl,
               Node_Edge_Types.Edge_Type_Renames);
         end;
      end if;
   end Extract_Edges;

end Extraction.Renamings;
