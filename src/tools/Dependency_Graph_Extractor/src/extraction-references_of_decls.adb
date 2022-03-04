with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.References_Of_Decls is

   use type LAL.Analysis_Unit;
   use type LALCO.Ada_Node_Kind_Type;

   function Is_Node_In_End_Name (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Node_In_End_Name (Node : LAL.Ada_Node'Class) return Boolean is
      Parent : constant LAL.Ada_Node := Node.Parent;
   begin
      if Parent.Kind not in LALCO.Ada_Dotted_Name then
         return
           Parent.Kind = LALCO.Ada_End_Name
           and then Parent.As_End_Name.F_Name = Node;
      else
         return Is_Node_In_End_Name (Parent);
      end if;
   end Is_Node_In_End_Name;

   function Is_Relevant_Base_Id (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Relevant_Base_Id (Node : LAL.Ada_Node'Class) return Boolean is
   begin
      return
        Node.Kind in LALCO.Ada_Base_Id
        and then not Is_Node_In_End_Name
          (Node) -- End names are syntactic sugar.
        and then not Node.As_Name.P_Is_Defining
        and then not Utilities.Get_Referenced_Decl (Node.As_Name).Is_Null
        and then Utilities.Is_Relevant_Basic_Decl
          (Utilities.Get_Referenced_Decl (Node.As_Name));
   end Is_Relevant_Base_Id;

   function Is_Node_In_With_Clause (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Node_In_With_Clause (Node : LAL.Ada_Node'Class) return Boolean
   is
      Parent : constant LAL.Ada_Node := Node.Parent;
   begin
      if Parent.Kind not in LALCO.Ada_Dotted_Name then
         return
           Parent.Kind = LALCO.Ada_Name_List
           and then Parent.Parent.Kind = LALCO.Ada_With_Clause
           and then Parent.Parent.As_With_Clause.F_Packages = Parent;
      else
         return Is_Node_In_With_Clause (Parent);
      end if;
   end Is_Node_In_With_Clause;

   function Is_Node_In_Subunit_Name (Node : LAL.Ada_Node'Class) return Boolean;
   function Is_Node_In_Subunit_Name (Node : LAL.Ada_Node'Class) return Boolean
   is
      Parent : constant LAL.Ada_Node := Node.Parent;
   begin
      if Parent.Kind not in LALCO.Ada_Dotted_Name then
         return
           Parent.Kind = LALCO.Ada_Subunit
           and then Parent.As_Subunit.F_Name = Node;
      else
         return Is_Node_In_Subunit_Name (Parent);
      end if;
   end Is_Node_In_Subunit_Name;

   function Is_Generic_Instantiation
     (Node : LAL.Ada_Node'Class) return Boolean is
     (Node.Kind in LALCO.Ada_Generic_Package_Instantiation |
          LALCO.Ada_Generic_Subp_Instantiation);

   function Get_Param_Instantiations
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Param_Actual_Array;
   function Get_Param_Instantiations
     (Basic_Decl : LAL.Basic_Decl'Class) return LAL.Param_Actual_Array
   is
   begin
      case LALCO.Ada_Basic_Decl (Basic_Decl.Kind) is
         when LALCO.Ada_Generic_Package_Instantiation =>
            return
              Basic_Decl.As_Generic_Package_Instantiation.F_Params
                .P_Zip_With_Params;
         when LALCO.Ada_Generic_Subp_Instantiation =>
            return
              Basic_Decl.As_Generic_Subp_Instantiation.F_Params
                .P_Zip_With_Params;
         when others =>
            raise Internal_Extraction_Error
              with "Cases in Is_Generic_Instantiation and "
              & "Get_Param_Instantiations do not match";
      end case;
   end Get_Param_Instantiations;

   procedure Extract_Nodes
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Is_Relevant_Base_Id (Node) then
         declare
            Name        : constant LAL.Name          := Node.As_Name;
            Target_Name : constant LAL.Defining_Name :=
              Utilities.Get_Referenced_Defining_Name (Name);
            Target_Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Referenced_Decl (Name);
         begin
            Graph.Write_Node (Target_Name, Target_Decl);
         end;
      elsif Is_Generic_Instantiation (Node) then
         declare
            Instantiation : constant LAL.Basic_Decl := Node.As_Basic_Decl;
         begin
            for Param of Get_Param_Instantiations (Instantiation) loop
               declare
                  Actual : constant LAL.Expr'Class := LAL.Actual (Param);
               begin
                  if not Actual.Is_Null
                    and then Actual.Kind = LALCO.Ada_Defining_Name
                  then
                     declare
                        Target_Name : constant LAL.Defining_Name :=
                          Actual.As_Defining_Name;
                        Target_Decl : constant LAL.Basic_Decl :=
                          Utilities.Get_Parent_Basic_Decl (Actual);
                     begin
                        Graph.Write_Node (Target_Name, Target_Decl);
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;
   end Extract_Nodes;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Is_Relevant_Base_Id (Node)
        and then not Is_Node_In_With_Clause
          (Node) -- Handled by Extraction.With_Clauses
      then
         declare
            Name        : constant LAL.Name          := Node.As_Name;
            Target_Name : constant LAL.Defining_Name :=
              Utilities.Get_Referenced_Defining_Name (Name);
            Target_Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Referenced_Decl (Name);
         begin
            --  Separately handle the parent name of separately compiled
            --  package, subprogram, or task, i.e., any descendant node of the
            --  F_Name field of a Subunit node. Get_Parent_Basic_Decl, used in
            --  the definition of Source below, yields null for such names.
            if not Is_Node_In_Subunit_Name (Name) then
               declare
                  Source_Decl : constant LAL.Basic_Decl :=
                    Utilities.Get_Parent_Basic_Decl (Name);
               begin
                  --  References at the top-level of a compilation unit, e.g.,
                  --  use clauses and pragmas, will have the Standard unit as
                  --  their parent. As it may confuse users to see references
                  --  from the Standard unit to their own code, we instead
                  --  use the file in which the reference occurs as the source.
                  if Name.Unit = Name.P_Standard_Unit
                    or else Source_Decl.Unit /= Source_Decl.P_Standard_Unit
                  then
                     Graph.Write_Edge
                       (Source_Decl, Target_Name, Target_Decl,
                        Node_Edge_Types.Edge_Type_References);
                  else
                     Graph.Write_Edge
                       (Name.Unit, Target_Name, Target_Decl,
                        Node_Edge_Types.Edge_Type_References);
                  end if;
               end;
            else
               Graph.Write_Edge
                 (Name.Unit, Target_Name, Target_Decl,
                  Node_Edge_Types.Edge_Type_References);
            end if;
         end;
      elsif Is_Generic_Instantiation (Node) then
         declare
            Instantiation : constant LAL.Basic_Decl := Node.As_Basic_Decl;
         begin
            for Param of Get_Param_Instantiations (Instantiation) loop
               declare
                  Actual : constant LAL.Expr'Class := LAL.Actual (Param);
               begin
                  if not Actual.Is_Null
                    and then Actual.Kind = LALCO.Ada_Defining_Name
                  then
                     declare
                        Target_Name : constant LAL.Defining_Name :=
                          Actual.As_Defining_Name;
                        Target_Decl : constant LAL.Basic_Decl :=
                          Utilities.Get_Parent_Basic_Decl (Actual);
                     begin
                        Graph.Write_Edge
                          (Instantiation, Target_Name, Target_Decl,
                           Node_Edge_Types.Edge_Type_References);
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;
   end Extract_Edges;

end Extraction.References_Of_Decls;
