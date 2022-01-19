with GNAT.Regpat;         use GNAT.Regpat;
with Rejuvenation.Finder; use Rejuvenation.Finder;
with Rejuvenation.Utils;  use Rejuvenation.Utils;

package body Rejuvenation.Placeholders is

   Placeholder_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
     Compile ("\$[SM]_[a-zA-Z0-9_]+");

   function Is_Placeholder_Name (Name : String) return Boolean is
     (GNAT.Regpat.Match (Placeholder_Matcher, Name));

   Single_Placeholder_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
     Compile ("\$S_[a-zA-Z0-9_]+");

   function Is_Single_Placeholder_Name (Name : String) return Boolean is
     (GNAT.Regpat.Match (Single_Placeholder_Matcher, Name));

   Multiple_Placeholder_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
     Compile ("\$M_[a-zA-Z0-9_]+");

   function Is_Multiple_Placeholder_Name (Name : String) return Boolean is
     (GNAT.Regpat.Match (Multiple_Placeholder_Matcher, Name));

   function Get_Name_At_Placeholder_Location
     (Node : Ada_Node'Class) return String;
   --  return the string at the location of a placeholder name
   --  in case of absence of placeholder location, the empty string is returned
   --
   --  Get_Name_At_Placeholder_Location
   --  is introduced to localize the information
   --  * what kind of nodes can be placeholders
   --  * where is the placeholder name located for the different kinds

   function Get_Name_At_Placeholder_Location
     (Node : Ada_Node'Class) return String
   is
   begin
      case Node.Kind is
         when Ada_Identifier | Ada_Defining_Name | Ada_Enum_Literal_Decl =>
            return Raw_Signature (Node);

         when Ada_Call_Stmt =>
            declare
               Name : constant Libadalang.Analysis.Name :=
                 Node.As_Call_Stmt.F_Call;
            begin
               if Name.Kind = Ada_Identifier then
                  return Raw_Signature (Name);
               end if;
            end;

         when Ada_Discriminant_Assoc =>
            declare
               D_A : constant Discriminant_Assoc := Node.As_Discriminant_Assoc;
               Ids        : constant Discriminant_Choice_List := D_A.F_Ids;
               Discr_Expr : constant Expr := D_A.F_Discr_Expr;
            begin
               if Ids.Children_Count = 0
                 and then Discr_Expr.Kind = Ada_Identifier
               then
                  return Raw_Signature (Discr_Expr);
               end if;
            end;

         when Ada_Param_Assoc =>
            declare
               P_A        : constant Param_Assoc := Node.As_Param_Assoc;
               Designator : constant Ada_Node    := P_A.F_Designator;
               R_Expr     : constant Expr        := P_A.F_R_Expr;
            begin
               if Designator.Is_Null and then R_Expr.Kind = Ada_Identifier then
                  return Raw_Signature (R_Expr);
               end if;
            end;

         when Ada_Aspect_Assoc =>
            declare
               A_A : constant Aspect_Assoc := Node.As_Aspect_Assoc;
               N   : constant Name         := A_A.F_Id;
            begin
               if A_A.F_Expr.Is_Null and then N.Kind = Ada_Identifier then
                  return Raw_Signature (N);
               end if;
            end;

         when Ada_Subtype_Indication =>
            declare
               SI : constant Subtype_Indication := Node.As_Subtype_Indication;
               Name : constant Libadalang.Analysis.Name := SI.F_Name;
            begin
               if Name.Kind = Ada_Identifier and then SI.F_Constraint.Is_Null
               then
                  return Raw_Signature (Name);
               end if;
            end;

         when others =>
            null;
      end case;
      return "";
   end Get_Name_At_Placeholder_Location;

   function Is_Placeholder (Node : Ada_Node'Class) return Boolean is
     (Is_Placeholder_Name (Get_Name_At_Placeholder_Location (Node)));

   function Is_Single_Placeholder (Node : Ada_Node'Class) return Boolean is
     (Is_Single_Placeholder_Name (Get_Name_At_Placeholder_Location (Node)));

   function Is_Multiple_Placeholder (Node : Ada_Node'Class) return Boolean is
     (Is_Multiple_Placeholder_Name (Get_Name_At_Placeholder_Location (Node)));

   function Get_Placeholder_Name (Node : Ada_Node'Class) return String is
     (Get_Name_At_Placeholder_Location (Node));

   function Get_Placeholders (Node : Ada_Node'Class) return Node_List.Vector is
     (Find_Non_Contained (Node, Is_Placeholder'Access));

   function Get_Placeholder_Names
     (Node : Ada_Node'Class) return String_Sets.Set
   is
      Result : String_Sets.Set;

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Is_Placeholder (Node) then
            Result.Include (Get_Placeholder_Name (Node));
            return Over;
         else
            return Into;
         end if;
      end Visit;

   begin
      Node.Traverse (Visit'Access);
      return Result;
   end Get_Placeholder_Names;

end Rejuvenation.Placeholders;
