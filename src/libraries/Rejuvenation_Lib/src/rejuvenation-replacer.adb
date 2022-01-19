with Ada.Assertions;       use Ada.Assertions;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Rejuvenation;         use Rejuvenation;
with Rejuvenation.Finder;  use Rejuvenation.Finder;
with Rejuvenation.Utils;   use Rejuvenation.Utils;
with String_Vectors;       use String_Vectors;
with String_Vectors_Utils; use String_Vectors_Utils;

package body Rejuvenation.Replacer is

   function Is_Empty
     (Node : Ada_Node'Class; Replacements : Map) return Boolean
     with
       Pre => not Node.Is_Null;

   function Is_Empty
     (Node : Ada_Node'Class; Replacements : Map) return Boolean
   is
   begin
      return
        Is_Placeholder (Node)
        and then Replacements.Element (Get_Placeholder_Name (Node)) = "";
   end Is_Empty;

   function Present_And_Empty
     (Node : Ada_Node'Class; Replacements : Map) return Boolean is
     (not Node.Is_Null and then Is_Empty (Node, Replacements));

   function Is_Empty_List
     (List : Ada_List'Class; Replacements : Map) return Boolean;
   function Is_Empty_List
     (List : Ada_List'Class; Replacements : Map) return Boolean
   is
   begin
      return
        (for all Child of List.Children => Is_Empty (Child, Replacements));
   end Is_Empty_List;

   function Is_Replacement_Node
     (Node : Ada_Node'Class; Replacements : Map) return Boolean;

   function Is_Replacement_Node
     (Node : Ada_Node'Class; Replacements : Map) return Boolean
   is
   begin
      if Is_Placeholder (Node) then
         return True;
      end if;

      case Node.Kind is
         when Ada_Object_Decl =>
            declare
               O_D : constant Object_Decl := Node.As_Object_Decl;
            begin
               return
                 Is_Empty_List (O_D.F_Ids, Replacements)
                 or else Present_And_Empty (O_D.F_Default_Expr, Replacements);
            end;
         when Ada_Call_Expr =>
            declare
               C_E    : constant Call_Expr := Node.As_Call_Expr;
               Suffix : constant Ada_Node  := C_E.F_Suffix;
            begin
               return
                 Suffix.Kind = Ada_Assoc_List
                 and then Is_Empty_List
                   (C_E.F_Suffix.As_Ada_List, Replacements);
            end;
         when Ada_If_Stmt =>
            declare
               I_S : constant If_Stmt := Node.As_If_Stmt;
            begin
               return Is_Empty_List (I_S.F_Else_Stmts, Replacements);
            end;
         when Ada_Decl_Block =>
            declare
               D_B   : constant Decl_Block       := Node.As_Decl_Block;
               Decls : constant Declarative_Part := D_B.F_Decls;
            begin
               --  Check for empty declarative part: "declare begin .. end;"
               return
                 not Decls.Is_Null
                 and then
                 (for all Child of Decls.F_Decls =>
                    Child.Kind = Ada_Object_Decl
                    and then Is_Empty_List
                      (Child.As_Object_Decl.F_Ids, Replacements));
            end;
         when Ada_Param_Assoc =>
            declare
               P_A : constant Param_Assoc := Node.As_Param_Assoc;
            begin
               return Present_And_Empty (P_A.F_Designator, Replacements);
            end;
         when Ada_Stmt_List =>  --  TODO: can lists be combined?
            declare
               S_L : constant Stmt_List := Node.As_Stmt_List;
            begin
   --  An empty stmt within a statement list is fine
   --  Only a completely empty statement list is problematic in some contexts
   --  including
               --    * if then else <empty> end if
               --    * when others => <empty>
               --    * begin <empty> end;
               return Is_Empty_List (S_L, Replacements);
            end;
         when Ada_Assoc_List | Ada_Aspect_Assoc_List =>
            declare
               A_L : constant Ada_List := Node.As_Ada_List;
            begin
               --  When a child is empty, also a separator must be removed
               return
                 (for some Child of A_L.Children =>
                    Is_Empty (Child, Replacements));
            end;
         when Ada_Aspect_Spec =>
            declare
               A_S : constant Aspect_Spec := Node.As_Aspect_Spec;
            begin
--  When the list of aspects is empty, also the 'with' keyword must be removed
               return Is_Empty_List (A_S.F_Aspect_Assocs, Replacements);
            end;
         when others =>
            return False;
      end case;
   end Is_Replacement_Node;

   function Get_Replacement_For_Node
     (Node : Ada_Node'Class; Replacements : Map) return String;
   function Get_Replacement_For_Node
     (Node : Ada_Node'Class; Replacements : Map) return String
   is
   begin
      if Is_Placeholder (Node) then
         return Replacements.Element (Get_Placeholder_Name (Node));
      end if;

      case Node.Kind is
         when Ada_Object_Decl =>
            declare
               O_D : constant Object_Decl := Node.As_Object_Decl;
               Ids : String_Vectors.Vector;
            begin
               for Id of O_D.F_Ids loop
                  declare
                     Value : constant String := Replace (Id, Replacements);
                  begin
                     if Value /= "" then
                        Ids.Append (Value);
                     end if;
                  end;
               end loop;

               if Ids.Is_Empty then
                  return "";
               else
                  declare
                     Start : constant String :=
                       Join (Ids, ", ") & " : " &
                       (if O_D.F_Has_Aliased then "aliased " else "") &
                       (if O_D.F_Has_Constant then "constant " else "") &
                       Replace (O_D.F_Type_Expr, Replacements) & " ";
                     Default_Expr : constant String :=
                       Replace (O_D.F_Default_Expr, Replacements);
                     Default_Expr_Tokens : constant String :=
                       (if Default_Expr = "" then ""
                        else ":= " & Default_Expr & " ");
                     Aspects : constant String :=
                       Replace (O_D.F_Aspects, Replacements);
                  begin
                     return Start & Default_Expr_Tokens & Aspects & ";";
                  end;
               end if;
            end;
         when Ada_Call_Expr =>
            declare
               C_E    : constant Call_Expr := Node.As_Call_Expr;
               Name   : constant String := Replace (C_E.F_Name, Replacements);
               Suffix : constant String    :=
                 (if C_E.F_Suffix.Kind = Ada_Assoc_List then
                    (if Is_Empty_List (C_E.F_Suffix.As_Ada_List, Replacements)
                     then ""
                     else "(" & Replace (C_E.F_Suffix, Replacements) & ")")
                  else Replace (C_E.F_Suffix, Replacements));
            begin
               return Name & Suffix;
            end;
         when Ada_If_Stmt =>
            declare
               I_S       : constant If_Stmt := Node.As_If_Stmt;
               Cond_Expr : constant String  :=
                 Replace (I_S.F_Cond_Expr, Replacements);
               Then_Stmts : constant String :=
                 Replace (I_S.F_Then_Stmts, Replacements);
               Else_Stmts : constant String :=
                 (if Is_Empty_List (I_S.F_Else_Stmts, Replacements) then ""
                  else " else " & Replace (I_S.F_Else_Stmts, Replacements));
               Alternatives : String_Vectors.Vector;
            begin
               for Alternative of I_S.F_Alternatives loop
                  Alternatives.Append (Replace (Alternative, Replacements));
               end loop;
               return
                 "if " & Cond_Expr & " then " & Then_Stmts &
                 Join (Alternatives) & Else_Stmts & " end if;";
            end;
         when Ada_Decl_Block =>
            declare
               D_B   : constant Decl_Block := Node.As_Decl_Block;
               Decls : String_Vectors.Vector;
            begin
               for Decl of D_B.F_Decls.F_Decls loop
                  declare
                     Value : constant String := Replace (Decl, Replacements);
                  begin
                     if Value /= "" then
                        Decls.Append (Value);
                     end if;
                  end;
               end loop;
               return
                 (if Decls.Is_Empty then "" else "declare " & Join (Decls)) &
                 " begin " & Replace (D_B.F_Stmts, Replacements) & " end;";
            end;
         when Ada_Param_Assoc =>
            declare
               P_A        : constant Param_Assoc := Node.As_Param_Assoc;
               Designator : constant String      :=
                 Replace (P_A.F_Designator, Replacements);
               R_Expr : constant String :=
                 Replace (P_A.F_R_Expr, Replacements);
               Designator_Tokens : constant String :=
                 (if Designator = "" then "" else Designator & " => ");
            begin
               return Designator_Tokens & R_Expr;
            end;
         when Ada_Aspect_Spec =>
            declare
               A_S           : constant Aspect_Spec := Node.As_Aspect_Spec;
               Aspect_Assocs : String_Vectors.Vector;
            begin
               for Aspect_Assoc of A_S.F_Aspect_Assocs loop
                  declare
                     Value : constant String :=
                       Replace (Aspect_Assoc, Replacements);
                  begin
                     if Value /= "" then
                        Aspect_Assocs.Append (Value);
                     end if;
                  end;
               end loop;

               return
                 (if Aspect_Assocs.Is_Empty then ""
                  else "with " & Join (Aspect_Assocs, ", "));
            end;
         when Ada_Stmt_List =>
            declare
               S_L   : constant Stmt_List := Node.As_Stmt_List;
               Stmts : String_Vectors.Vector;
            begin
               for Child of S_L.Children loop
                  declare
                     Value : constant String := Replace (Child, Replacements);
                  begin
                     if Value /= "" then
                        Stmts.Append (Value);
                     end if;
                  end;
               end loop;
               return
                 (if Stmts.Is_Empty then "null;"
                  else Join (Stmts, (1 => ASCII.LF)));
            end;
         when Ada_Assoc_List | Ada_Aspect_Assoc_List =>
            declare
               A_L    : constant Ada_List := Node.As_Ada_List;
               Values : String_Vectors.Vector;
            begin
               for Child of A_L.Children loop
                  declare
                     Value : constant String := Replace (Child, Replacements);
                  begin
                     if Value /= "" then
                        Values.Append (Value);
                     end if;
                  end;
               end loop;
               return Join (Values, ", ");
            end;
         when others =>
            Assert
              (Check   => False,
               Message =>
                 "Get_Replacement_For_Node: Unexpected kind " &
                 Node.Kind'Image);
            return "";
      end case;
   end Get_Replacement_For_Node;

   function Get_Nodes_To_Be_Replaced
     (Node : Ada_Node'Class; Replacements : Map) return Node_List.Vector;

   function Get_Nodes_To_Be_Replaced
     (Node : Ada_Node'Class; Replacements : Map) return Node_List.Vector
   is

      function Predicate (Node : Ada_Node'Class) return Boolean is
        (Is_Replacement_Node (Node, Replacements));

   begin
      return Find_Non_Contained (Node, Predicate'Access);
   end Get_Nodes_To_Be_Replaced;

   function Tail (V : Node_List.Vector) return Node_List.Vector;
   function Tail (V : Node_List.Vector) return Node_List.Vector
   is
      Return_Value : Node_List.Vector := V.Copy;
   begin
      Node_List.Delete_First (Return_Value);
      return Return_Value;
   end Tail;

   function Replace
     (Original     : String; Nodes_To_Be_Replaced : Node_List.Vector;
      Replacements : Map) return String;
   function Replace
     (Original     : String; Nodes_To_Be_Replaced : Node_List.Vector;
      Replacements : Map) return String
   is
   begin
      if Nodes_To_Be_Replaced.Is_Empty then
         return Original;
      end if;
      declare
         Node_To_Be_Replaced : constant Ada_Node'Class :=
           Nodes_To_Be_Replaced.First_Element;
         R_S : constant String := Raw_Signature (Node_To_Be_Replaced);
         First                  : constant Natural := Index (Original, R_S);
         Last                   : constant Natural := First + R_S'Length - 1;
         Replacement_Nodes_Tail : constant Node_List.Vector :=
           Tail (Nodes_To_Be_Replaced);
      begin
         Assert
           (Check   => First /= 0,
            Message => "Replacement_Node unexpectedly not found");
         declare
            Prefix    : String renames Original (Original'First .. First - 1);
            Remainder : String renames Original (Last + 1 .. Original'Last);
            Insert    : constant String :=
              Get_Replacement_For_Node (Node_To_Be_Replaced, Replacements);
            Tail : constant String :=
              Replace (Remainder, Replacement_Nodes_Tail, Replacements);
         begin
            return Prefix & Insert & Tail;
         end;
      end;
   end Replace;

   function Replace (Node : Ada_Node'Class; Replacements : Map) return String
   is
      Nodes_To_Be_Replaced : constant Node_List.Vector :=
        Get_Nodes_To_Be_Replaced (Node, Replacements);
   begin
      return
        Replace (Raw_Signature (Node), Nodes_To_Be_Replaced, Replacements);
   end Replace;

end Rejuvenation.Replacer;
