with Ada.Assertions;            use Ada.Assertions;
with Ada.Text_IO;               use Ada.Text_IO;
with GNATCOLL.GMP.Integers;     use GNATCOLL.GMP.Integers;
with Langkit_Support.Text;      use Langkit_Support.Text;
with Rejuvenation.Placeholders; use Rejuvenation.Placeholders;
with Rejuvenation.Utils;        use Rejuvenation.Utils;

package body Rejuvenation.Match_Patterns is

   -- Public: Create match --------

   function Match_Full
     (MP : out Match_Pattern; Pattern : Ada_Node; Instance : Ada_Node)
      return Boolean
   is
      Pattern_Array, Instance_Array : Ada_Node_Array (1 .. 1);
   begin
      if Pattern.Is_Null or else Instance.Is_Null then
         return False;
      else
         Pattern_Array (1)  := Pattern;
         Instance_Array (1) := Instance;
         return MP.Match_Full (Pattern_Array, Instance_Array);
      end if;
   end Match_Full;

   function Match_Full
     (MP       : out Match_Pattern; Pattern : Ada_Node_Array;
      Instance :     Ada_Node_Array) return Boolean
   is
   begin
      return MP.Match (Pattern, Instance, Instance'First, True, True);
   end Match_Full;

   function Match_Prefix
     (MP       : out Match_Pattern; Pattern : Ada_Node_Array;
      Instance : Ada_Node_Array; Instance_Start_Index : Integer) return Boolean
   is
   begin
      return MP.Match (Pattern, Instance, Instance_Start_Index, False, True);
   end Match_Prefix;

   function Are_Identical
     (Node1 : Ada_Node'Class; Node2 : Ada_Node'Class) return Boolean
   is
      MP      : Match_Pattern;
      Success : constant Boolean :=
        MP.Match_Full (Node1.As_Ada_Node, Node2.As_Ada_Node);
   begin
      if not MP.Mapping_Single.Is_Empty
        or else not MP.Mapping_Multiple.Is_Empty
      then
         raise Unsupported_Placeholder_Exception;
      end if;
      return Success;
   end Are_Identical;

   -- Public: Inspect match --------

   function Get_Nodes (MP : Match_Pattern) return Node_List.Vector is
   begin
      return MP.Nodes;
   end Get_Nodes;

   function Has_Single
     (MP : Match_Pattern; Placeholder_Name : String) return Boolean
   is
   begin
      return MP.Mapping_Single.Contains (Placeholder_Name);
   end Has_Single;

   function Get_Single_As_Node
     (MP : Match_Pattern; Placeholder_Name : String) return Ada_Node
   is
   begin
      if not MP.Mapping_Single.Contains (Placeholder_Name) then
         Dump_Partial_Match (MP);
      end if;

      Assert
        (Check   => MP.Mapping_Single.Contains (Placeholder_Name),
         Message => "Mapping_Single doesn't contain " & Placeholder_Name);
      return MP.Mapping_Single.Element (Placeholder_Name);
   end Get_Single_As_Node;

   function Get_Single_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String
   is
      Node : constant Ada_Node := MP.Get_Single_As_Node (Placeholder_Name);
   begin
      return Raw_Signature (Node);
   end Get_Single_As_Raw_Signature;

   function Has_Multiple
     (MP : Match_Pattern; Placeholder_Name : String) return Boolean
   is
   begin
      return MP.Mapping_Multiple.Contains (Placeholder_Name);
   end Has_Multiple;

   function Get_Multiple_As_Nodes
     (MP : Match_Pattern; Placeholder_Name : String) return Node_List.Vector
   is
   begin
      return MP.Mapping_Multiple.Element (Placeholder_Name);
   end Get_Multiple_As_Nodes;

   function Get_Multiple_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String
   is
      Nodes : constant Node_List.Vector :=
        Get_Multiple_As_Nodes (MP, Placeholder_Name);
   begin
      if Nodes.Length = 0 then
         return "";
      else
         declare
            First_Node : constant Ada_Node        := Nodes.First_Element;
            Last_Node  : constant Ada_Node        := Nodes.Last_Element;
            FirstToken : constant Token_Reference := First_Node.Token_Start;
            LastToken  : constant Token_Reference := Last_Node.Token_End;
            Unit       : constant Analysis_Unit   := First_Node.Unit;
            First : constant Integer := Raw_Data (FirstToken).Source_First;
            Last       : constant Integer := Raw_Data (LastToken).Source_Last;
         begin
            return Encode (Unit.Text (First .. Last), Unit.Get_Charset);
         end;
      end if;
   end Get_Multiple_As_Raw_Signature;

   function Get_Placeholder_As_Nodes
     (MP : Match_Pattern; Placeholder_Name : String) return Node_List.Vector
   is
   begin
      if Is_Single_Placeholder_Name (Placeholder_Name) then
         return
           Node_List.To_Vector (Get_Single_As_Node (MP, Placeholder_Name), 1);
      else
         Assert
           (Check   => Is_Multiple_Placeholder_Name (Placeholder_Name),
            Message =>
              "Illegal Placeholder_Name [not (single or multiple)] : " &
              Placeholder_Name);
         return Get_Multiple_As_Nodes (MP, Placeholder_Name);
      end if;
   end Get_Placeholder_As_Nodes;

   function Get_Placeholder_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String
   is
   begin
      if Is_Single_Placeholder_Name (Placeholder_Name) then
         return Get_Single_As_Raw_Signature (MP, Placeholder_Name);
      else
         Assert
           (Check   => Is_Multiple_Placeholder_Name (Placeholder_Name),
            Message =>
              "Illegal Placeholder_Name [not (single or multiple)] : " &
              Placeholder_Name);
         return Get_Multiple_As_Raw_Signature (MP, Placeholder_Name);
      end if;
   end Get_Placeholder_As_Raw_Signature;

   -- Private: Multiple_Placeholder_Status --------

   function Is_Open (MPS : Multiple_Placeholder_Status) return Boolean is
   begin
      return MPS.Ongoing_Multiple;
   end Is_Open;

   procedure Open
     (MPS : in out Multiple_Placeholder_Status; MP : Match_Pattern;
      Placeholder_Node :        Ada_Node)
   is
   begin
      if Is_Open (MPS) then
         raise Invalid_Multiple_Placeholder_Status_Exception;
      end if;
      MPS.Ongoing_Multiple          := True;
      MPS.Multiple_PlaceHolder_Name := Placeholder_Node;
      MPS.Multiple_Placeholder_Nodes.Clear;
      MPS.Has_Earlier_Multiple_Placeholder_Nodes :=
        MP.Mapping_Multiple.Contains
          (Get_Placeholder_Name (MPS.Multiple_PlaceHolder_Name));
      if MPS.Has_Earlier_Multiple_Placeholder_Nodes then
         MPS.Earlier_Multiple_Placeholder_Nodes :=
           MP.Mapping_Multiple.Element
             (Get_Placeholder_Name (MPS.Multiple_PlaceHolder_Name));
      end if;
   end Open;

   procedure Close
     (MPS : in out Multiple_Placeholder_Status; MP : in out Match_Pattern)
   is
   begin
      if not Is_Open (MPS) then
         raise Invalid_Multiple_Placeholder_Status_Exception;
      end if;
      if MPS.Has_Earlier_Multiple_Placeholder_Nodes
        and then MPS.Earlier_Multiple_Placeholder_Nodes.Length /=
          MPS.Multiple_Placeholder_Nodes.Length
      then
         raise Inconsistent_Placeholder_Values_Exception
           with Get_Placeholder_Name (MPS.Multiple_PlaceHolder_Name);
      else
         MP.Mapping_Multiple.Include
           (Get_Placeholder_Name (MPS.Multiple_PlaceHolder_Name),
            MPS.Multiple_Placeholder_Nodes);
         MPS.Ongoing_Multiple := False;
      end if;
   end Close;

   procedure Update
     (MPS : in out Multiple_Placeholder_Status; Instance_Node : Ada_Node)
   is
   begin
      if not Is_Open (MPS) then
         raise Invalid_Multiple_Placeholder_Status_Exception;
      end if;
      if MPS.Has_Earlier_Multiple_Placeholder_Nodes then
         if MPS.Earlier_Multiple_Placeholder_Nodes.Length <=
           MPS.Multiple_Placeholder_Nodes.Length
           or else not Are_Equal_In_Ada
             (MPS.Earlier_Multiple_Placeholder_Nodes.Element
                (Integer (MPS.Multiple_Placeholder_Nodes.Length + 1)),
              Instance_Node)
         then
            raise Inconsistent_Placeholder_Values_Exception
              with Get_Placeholder_Name (MPS.Multiple_PlaceHolder_Name) &
              " at " & Instance_Node.Image;
         end if;
      end if;
      MPS.Multiple_Placeholder_Nodes.Append (Instance_Node);
   end Update;

   -- Private: Create match --------

   function Has_Nested_Match_Full
     (MP : Match_Pattern; Pattern : Ada_Node; Instance : Ada_Node)
      return Boolean
   is
      Nested_MP : Match_Pattern;
   begin
      for E in MP.Mapping_Single.Iterate loop
         Nested_MP.Mapping_Single.Include
           (Mapping_Single_Map.Key (E), Mapping_Single_Map.Element (E));
      end loop;
      for E in MP.Mapping_Multiple.Iterate loop
         Nested_MP.Mapping_Multiple.Include
           (Mapping_Multiple_Map.Key (E), Mapping_Multiple_Map.Element (E));
      end loop;
      return Nested_MP.Match (Pattern, Instance);
   exception
      when Inconsistent_Placeholder_Values_Exception =>
         return False;
   end Has_Nested_Match_Full;

   function Match
     (MP : in out Match_Pattern; Pattern : Ada_Node_Array;
      Instance :        Ada_Node_Array; Instance_Start_Index : Integer;
      Pattern_Must_Cover_End_Of_Instance :    Boolean; Store_Nodes : Boolean)
      return Boolean
   is
      Dummy          : Boolean;
      Instance_Index : Integer := Instance_Start_Index;
      MPS            : Multiple_Placeholder_Status;
   begin
      for Pattern_Index in Pattern'Range loop
         if Is_Multiple_Placeholder (Pattern (Pattern_Index)) then
            --  CASE: current pattern node IS a multiple placeholder
            if Is_Open (MPS) then
               Close (MPS, MP);
            end if;
            Open (MPS, MP, Pattern (Pattern_Index));
         else
            declare
               Old_Diagnose : constant Boolean := DIAGNOSE;
            begin
               DIAGNOSE := False;
               if Is_Open (MPS) then
--  CASE: previous pattern node IS a multiple placeholder,
--  and current pattern node IS NOT a multiple placeholder
--  Interpret instance nodes that do not match the current pattern node as part
--  of the previous multiple placeholder
                  while Instance_Index <= Instance'Last
                    and then not Has_Nested_Match_Full
                      (MP, Pattern (Pattern_Index), Instance (Instance_Index))
                  loop
                     Update (MPS, Instance (Instance_Index));
                     Instance_Index := Instance_Index + 1;
                  end loop;
                  Close (MPS, MP);
               else
   --  CASE: previous pattern node IS NOT a multiple placeholder, and current
   --  pattern node IS NOT a multiple placeholder
                  null;
               end if;
               DIAGNOSE := Old_Diagnose;
            end;

            if Instance_Index <= Instance'Last
              and then MP.Has_Nested_Match_Full
                (Pattern (Pattern_Index), Instance (Instance_Index))
            then
               Dummy :=
                 MP.Match (Pattern (Pattern_Index), Instance (Instance_Index));
               Instance_Index := Instance_Index + 1;
            else
               if DIAGNOSE and then Instance_Index > Instance'Last then
                  MP.Dump_Partial_Match;
                  Put_Line (Standard_Error, "Superfluous node in pattern:");
                  Put_Line
                    (Standard_Error,
                     "* Pattern " & Pattern (Pattern_Index).Image & ": " &
                     Raw_Signature (Pattern (Pattern_Index)));
               end if;
               return False;
            end if;
         end if;
      end loop;

      if Is_Open (MPS) then
         --  CASE: last pattern node IS a multiple placeholder
         --  Interpret instance nodes as part of the last multiple placeholder
         while Instance_Index <= Instance'Last loop
            Update (MPS, Instance (Instance_Index));
            Instance_Index := Instance_Index + 1;
         end loop;
         Close (MPS, MP);
      end if;

      --  Check whether the instance is longer than the pattern
      if not Pattern_Must_Cover_End_Of_Instance
        or else Instance_Index > Instance'Last
      then
         if Store_Nodes then
            for Index in Instance_Start_Index .. Instance_Index - 1 loop
               MP.Nodes.Append (Instance (Index));
            end loop;
         end if;
         return True;
      else
         if DIAGNOSE then
            MP.Dump_Partial_Match;
            Put_Line (Standard_Error, "Superfluous node in instance:");
            Put_Line
              (Standard_Error,
               "* Instance " & Instance (Instance_Index).Image & ": " &
               Raw_Signature (Instance (Instance_Index)));
         end if;
         return False;
      end if;
   end Match;

   function Match
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean
   is
   begin
      if Pattern.Is_Null and then Instance.Is_Null then
         return True;

      elsif Pattern.Is_Null then
         if DIAGNOSE then
            MP.Dump_Partial_Match;
            Put_Line (Standard_Error, "Superfluous node in instance:");
            Put_Line
              (Standard_Error,
               "* Instance " & Instance.Image & ": " &
               Raw_Signature (Instance));
         end if;
         return False;

      elsif Instance.Is_Null and then Is_Single_Placeholder (Pattern) then
         if DIAGNOSE then
            MP.Dump_Partial_Match;
            Put_Line (Standard_Error, "Superfluous node in single pattern:");
            Put_Line
              (Standard_Error,
               "* Pattern " & Pattern.Image & ": " & Raw_Signature (Pattern));
         end if;
         return False;
      end if;

      declare
         Success : Boolean;
      begin
         if Is_Single_Placeholder (Pattern) then
            Assert
              (Check   => not Instance.Is_Null,
               Message => "For single plaeholder: Instance can't be null");
            Success := MP.Match_Single_Placeholder (Pattern, Instance);
         elsif Is_Multiple_Placeholder (Pattern) then
            Success := MP.Match_Multiple_Placeholder (Pattern, Instance);
         else
            if Instance.Is_Null then
               if Pattern.Kind /= Ada_Aspect_Spec then
                  if DIAGNOSE then
                     MP.Dump_Partial_Match;
                     Put_Line (Standard_Error, "Superfluous node in pattern:");
                     Put_Line
                       (Standard_Error,
                        "* Pattern " & Pattern.Image & ": " &
                        Raw_Signature (Pattern));
                  end if;
                  return False;
               end if;

               declare
                  A_S : constant Aspect_Spec := Pattern.As_Aspect_Spec;
                  Empty_Ada_Node_Array : constant Ada_Node_Array (1 .. 0) :=
                    (others => No_Ada_Node);
               begin
                  Success :=
                    MP.Match
                      (A_S.F_Aspect_Assocs.Children, Empty_Ada_Node_Array, 1,
                       True, False);
               end;
            else
               Success := MP.Match_Specific (Pattern, Instance);
            end if;
         end if;

         if not Success then
            if DIAGNOSE then
               if Pattern.Kind = Instance.Kind then
                  Put_Line
                    (Standard_Error,
                     "Incompatible pattern and instance of the same kind:");
               else
                  Put_Line
                    (Standard_Error,
                     "Incompatible pattern and instance kinds:");
               end if;
               Put_Line
                 (Standard_Error,
                  "* Pattern " & Pattern.Image & ": " &
                  Raw_Signature (Pattern));
               Put_Line
                 (Standard_Error,
                  "* Instance " & Instance.Image & ": " &
                  Raw_Signature (Instance));
            end if;
         end if;
         return Success;
      end;
   end Match;

   function Match_Multiple_Placeholder
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean
   is
      Placeholder_Name : constant String := Get_Placeholder_Name (Pattern);
      Instance_Vector  : constant Node_List.Vector :=
        (if Instance.Is_Null then Node_List.Empty_Vector
         else Node_List.To_Vector (Ada_Node (Instance), 1));
   begin
      if MP.Has_Multiple (Placeholder_Name) then
         declare
            Earlier_Mapping : constant Node_List.Vector :=
              MP.Get_Multiple_As_Nodes (Placeholder_Name);
         begin
            if Earlier_Mapping.Length /= Instance_Vector.Length
              or else
              (for some I in 1 .. Natural (Instance_Vector.Length) =>
                 not Are_Equal_In_Ada
                   (Earlier_Mapping.Element (I), Instance_Vector.Element (I)))
            then
               raise Inconsistent_Placeholder_Values_Exception;
            end if;
         end;
      else
         MP.Mapping_Multiple.Include (Placeholder_Name, Instance_Vector);
      end if;
      return True;
   end Match_Multiple_Placeholder;

   function Match_Single_Placeholder
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean
   is

      function Disambiguate_Node (Node : Ada_Node) return Ada_Node;
      --  The same Ada code can be interpreted as different Nodes
      --  e.g. an defining name and an indentifier
      function Disambiguate_Node (Node : Ada_Node) return Ada_Node
      is
      begin
         if Node.Is_Null or else Node.Children_Count /= 1 then
            return Node;
         else
            return Disambiguate_Node (Node.First_Child);
               --  Robustness: it might happen multiple times.
         end if;
      end Disambiguate_Node;

      Placeholder_Name : constant String := Get_Placeholder_Name (Pattern);
   begin
      if MP.Has_Single (Placeholder_Name) then
         declare
            Earlier_Mapping : constant Ada_Node :=
              MP.Get_Single_As_Node (Placeholder_Name);
         begin
            if not Are_Equal_In_Ada
               (Disambiguate_Node (Earlier_Mapping),
                Disambiguate_Node (Instance.As_Ada_Node))
            then
               raise Inconsistent_Placeholder_Values_Exception;
            end if;
         end;
      else
         MP.Mapping_Single.Include (Placeholder_Name, Ada_Node (Instance));
      end if;
      return True;
   end Match_Single_Placeholder;

   function filter (Node : Ada_Node) return Ada_Node_Array;
   function filter (Node : Ada_Node) return Ada_Node_Array is
   begin
      if Node.Kind = Ada_Discriminant_Constraint then
         return Node.As_Discriminant_Constraint.F_Constraints.Children;
      elsif Node.Kind = Ada_Index_Constraint then
         return Node.As_Index_Constraint.F_Constraints.Children;
      else
         return (1 => Node);
      end if;
   end filter;

   function Match_Specific
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean
   is
   begin
      if Pattern.Kind in Ada_Discriminant_Constraint | Ada_Index_Constraint
        or else Instance.Kind in Ada_Discriminant_Constraint |
            Ada_Index_Constraint
      then
         declare
            filtered_Pattern : constant Ada_Node_Array :=
              filter (Pattern.As_Ada_Node);
            filtered_Instance : constant Ada_Node_Array :=
              filter (Instance.As_Ada_Node);
         begin
            return
              MP.Match
                (filtered_Pattern, filtered_Instance, filtered_Instance'First,
                 True, False);
         end;
      end if;

      if Pattern.Kind /= Instance.Kind then
         return False;

      -- Begin of generated fragment using Rejuvenation_Lib_Generator --------
      elsif Pattern.Kind = Ada_Protected_Type_Decl then
         declare
            Pattern2 : constant Protected_Type_Decl :=
              Pattern.As_Protected_Type_Decl;
            Instance2 : constant Protected_Type_Decl :=
              Instance.As_Protected_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match (Pattern2.F_Interfaces, Instance2.F_Interfaces)
              and then MP.Match
                (Pattern2.F_Definition, Instance2.F_Definition);
         end;
      elsif Pattern.Kind = Ada_Abstract_Formal_Subp_Decl then
         declare
            Pattern2 : constant Abstract_Formal_Subp_Decl :=
              Pattern.As_Abstract_Formal_Subp_Decl;
            Instance2 : constant Abstract_Formal_Subp_Decl :=
              Instance.As_Abstract_Formal_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr);
         end;
      elsif Pattern.Kind = Ada_Variant_Part then
         declare
            Pattern2  : constant Variant_Part := Pattern.As_Variant_Part;
            Instance2 : constant Variant_Part := Instance.As_Variant_Part;
         begin
            return
              MP.Match (Pattern2.F_Discr_Name, Instance2.F_Discr_Name)
              and then MP.Match (Pattern2.F_Variant, Instance2.F_Variant);
         end;
      elsif Pattern.Kind = Ada_Paren_Abstract_State_Decl then
         declare
            Pattern2 : constant Paren_Abstract_State_Decl :=
              Pattern.As_Paren_Abstract_State_Decl;
            Instance2 : constant Paren_Abstract_State_Decl :=
              Instance.As_Paren_Abstract_State_Decl;
         begin
            return MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_Case_Stmt_Alternative then
         declare
            Pattern2 : constant Case_Stmt_Alternative :=
              Pattern.As_Case_Stmt_Alternative;
            Instance2 : constant Case_Stmt_Alternative :=
              Instance.As_Case_Stmt_Alternative;
         begin
            return
              MP.Match (Pattern2.F_Choices, Instance2.F_Choices)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts);
         end;
      elsif Pattern.Kind = Ada_Generic_Formal_Package then
         declare
            Pattern2 : constant Generic_Formal_Package :=
              Pattern.As_Generic_Formal_Package;
            Instance2 : constant Generic_Formal_Package :=
              Instance.As_Generic_Formal_Package;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_Anonymous_Type then
         declare
            Pattern2  : constant Anonymous_Type := Pattern.As_Anonymous_Type;
            Instance2 : constant Anonymous_Type := Instance.As_Anonymous_Type;
         begin
            return MP.Match (Pattern2.F_Type_Decl, Instance2.F_Type_Decl);
         end;
      elsif Pattern.Kind = Ada_Type_Access_Def then
         declare
            Pattern2  : constant Type_Access_Def := Pattern.As_Type_Access_Def;
            Instance2 : constant Type_Access_Def :=
              Instance.As_Type_Access_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match (Pattern2.F_Has_All, Instance2.F_Has_All)
              and then MP.Match
                (Pattern2.F_Has_Constant, Instance2.F_Has_Constant)
              and then MP.Match
                (Pattern2.F_Subtype_Indication,
                 Instance2.F_Subtype_Indication);
         end;
      elsif Pattern.Kind = Ada_Aggregate_Assoc then
         declare
            Pattern2  : constant Aggregate_Assoc := Pattern.As_Aggregate_Assoc;
            Instance2 : constant Aggregate_Assoc :=
              Instance.As_Aggregate_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Designators, Instance2.F_Designators)
              and then MP.Match (Pattern2.F_R_Expr, Instance2.F_R_Expr);
         end;
      elsif Pattern.Kind = Ada_Discrete_Subtype_Name then
         declare
            Pattern2 : constant Discrete_Subtype_Name :=
              Pattern.As_Discrete_Subtype_Name;
            Instance2 : constant Discrete_Subtype_Name :=
              Instance.As_Discrete_Subtype_Name;
         begin
            return MP.Match (Pattern2.F_Subtype, Instance2.F_Subtype);
         end;
      elsif Pattern.Kind = Ada_Error_Decl then
         declare
            Pattern2  : constant Error_Decl := Pattern.As_Error_Decl;
            Instance2 : constant Error_Decl := Instance.As_Error_Decl;
         begin
            return MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects);
         end;
      elsif Pattern.Kind = Ada_Incomplete_Type_Decl then
         declare
            Pattern2 : constant Incomplete_Type_Decl :=
              Pattern.As_Incomplete_Type_Decl;
            Instance2 : constant Incomplete_Type_Decl :=
              Instance.As_Incomplete_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants);
         end;
      elsif Pattern.Kind = Ada_Case_Expr_Alternative then
         declare
            Pattern2 : constant Case_Expr_Alternative :=
              Pattern.As_Case_Expr_Alternative;
            Instance2 : constant Case_Expr_Alternative :=
              Instance.As_Case_Expr_Alternative;
         begin
            return
              MP.Match (Pattern2.F_Choices, Instance2.F_Choices)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Anonymous_Type_Decl then
         declare
            Pattern2 : constant Anonymous_Type_Decl :=
              Pattern.As_Anonymous_Type_Decl;
            Instance2 : constant Anonymous_Type_Decl :=
              Instance.As_Anonymous_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match (Pattern2.F_Type_Def, Instance2.F_Type_Def);
         end;
      elsif Pattern.Kind = Ada_Generic_Subp_Instantiation then
         declare
            Pattern2 : constant Generic_Subp_Instantiation :=
              Pattern.As_Generic_Subp_Instantiation;
            Instance2 : constant Generic_Subp_Instantiation :=
              Instance.As_Generic_Subp_Instantiation;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Kind, Instance2.F_Kind)
              and then MP.Match (Pattern2.F_Subp_Name, Instance2.F_Subp_Name)
              and then MP.Match
                (Pattern2.F_Generic_Subp_Name, Instance2.F_Generic_Subp_Name)
              and then MP.Match (Pattern2.F_Params, Instance2.F_Params);
         end;
      elsif Pattern.Kind = Ada_Decl_Expr then
         declare
            Pattern2  : constant Decl_Expr := Pattern.As_Decl_Expr;
            Instance2 : constant Decl_Expr := Instance.As_Decl_Expr;
         begin
            return
              MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Decimal_Fixed_Point_Def then
         declare
            Pattern2 : constant Decimal_Fixed_Point_Def :=
              Pattern.As_Decimal_Fixed_Point_Def;
            Instance2 : constant Decimal_Fixed_Point_Def :=
              Instance.As_Decimal_Fixed_Point_Def;
         begin
            return
              MP.Match (Pattern2.F_Delta, Instance2.F_Delta)
              and then MP.Match (Pattern2.F_Digits, Instance2.F_Digits)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Protected_Body then
         declare
            Pattern2  : constant Protected_Body := Pattern.As_Protected_Body;
            Instance2 : constant Protected_Body := Instance.As_Protected_Body;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Call_Expr then
         declare
            Pattern2  : constant Call_Expr := Pattern.As_Call_Expr;
            Instance2 : constant Call_Expr := Instance.As_Call_Expr;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Suffix, Instance2.F_Suffix);
         end;
      elsif Pattern.Kind = Ada_Named_Stmt_Decl then
         declare
            Pattern2  : constant Named_Stmt_Decl := Pattern.As_Named_Stmt_Decl;
            Instance2 : constant Named_Stmt_Decl :=
              Instance.As_Named_Stmt_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_While_Loop_Stmt then
         declare
            Pattern2  : constant While_Loop_Stmt := Pattern.As_While_Loop_Stmt;
            Instance2 : constant While_Loop_Stmt :=
              Instance.As_While_Loop_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Spec, Instance2.F_Spec)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Task_Body_Stub then
         declare
            Pattern2  : constant Task_Body_Stub := Pattern.As_Task_Body_Stub;
            Instance2 : constant Task_Body_Stub := Instance.As_Task_Body_Stub;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Subtype_Decl then
         declare
            Pattern2  : constant Subtype_Decl := Pattern.As_Subtype_Decl;
            Instance2 : constant Subtype_Decl := Instance.As_Subtype_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Subtype, Instance2.F_Subtype);
         end;
      elsif Pattern.Kind = Ada_Subp_Spec then
         declare
            Pattern2  : constant Subp_Spec := Pattern.As_Subp_Spec;
            Instance2 : constant Subp_Spec := Instance.As_Subp_Spec;
         begin
            return
              MP.Match (Pattern2.F_Subp_Kind, Instance2.F_Subp_Kind)
              and then MP.Match (Pattern2.F_Subp_Name, Instance2.F_Subp_Name)
              and then MP.Match
                (Pattern2.F_Subp_Params, Instance2.F_Subp_Params)
              and then MP.Match
                (Pattern2.F_Subp_Returns, Instance2.F_Subp_Returns);
         end;
      elsif Pattern.Kind = Ada_Package_Body then
         declare
            Pattern2  : constant Package_Body := Pattern.As_Package_Body;
            Instance2 : constant Package_Body := Instance.As_Package_Body;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Package_Name, Instance2.F_Package_Name)
              and then MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Iterated_Assoc then
         declare
            Pattern2  : constant Iterated_Assoc := Pattern.As_Iterated_Assoc;
            Instance2 : constant Iterated_Assoc := Instance.As_Iterated_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Spec, Instance2.F_Spec)
              and then MP.Match (Pattern2.F_R_Expr, Instance2.F_R_Expr);
         end;
      elsif Pattern.Kind = Ada_Anonymous_Type_Access_Def then
         declare
            Pattern2 : constant Anonymous_Type_Access_Def :=
              Pattern.As_Anonymous_Type_Access_Def;
            Instance2 : constant Anonymous_Type_Access_Def :=
              Instance.As_Anonymous_Type_Access_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match (Pattern2.F_Type_Decl, Instance2.F_Type_Decl);
         end;
      elsif Pattern.Kind = Ada_Enum_Type_Def then
         declare
            Pattern2  : constant Enum_Type_Def := Pattern.As_Enum_Type_Def;
            Instance2 : constant Enum_Type_Def := Instance.As_Enum_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Enum_Literals, Instance2.F_Enum_Literals);
         end;
      elsif Pattern.Kind = Ada_Delta_Constraint then
         declare
            Pattern2 : constant Delta_Constraint :=
              Pattern.As_Delta_Constraint;
            Instance2 : constant Delta_Constraint :=
              Instance.As_Delta_Constraint;
         begin
            return
              MP.Match (Pattern2.F_Digits, Instance2.F_Digits)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Interface_Type_Def then
         declare
            Pattern2 : constant Interface_Type_Def :=
              Pattern.As_Interface_Type_Def;
            Instance2 : constant Interface_Type_Def :=
              Instance.As_Interface_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Interface_Kind, Instance2.F_Interface_Kind)
              and then MP.Match
                (Pattern2.F_Interfaces, Instance2.F_Interfaces);
         end;
      elsif Pattern.Kind = Ada_Label_Decl then
         declare
            Pattern2  : constant Label_Decl := Pattern.As_Label_Decl;
            Instance2 : constant Label_Decl := Instance.As_Label_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Generic_Subp_Internal then
         declare
            Pattern2 : constant Generic_Subp_Internal :=
              Pattern.As_Generic_Subp_Internal;
            Instance2 : constant Generic_Subp_Internal :=
              Instance.As_Generic_Subp_Internal;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Delta_Aggregate then
         declare
            Pattern2  : constant Delta_Aggregate := Pattern.As_Delta_Aggregate;
            Instance2 : constant Delta_Aggregate :=
              Instance.As_Delta_Aggregate;
         begin
            return
              MP.Match (Pattern2.F_Ancestor_Expr, Instance2.F_Ancestor_Expr)
              and then MP.Match (Pattern2.F_Assocs, Instance2.F_Assocs);
         end;
      elsif Pattern.Kind = Ada_Entry_Spec then
         declare
            Pattern2  : constant Entry_Spec := Pattern.As_Entry_Spec;
            Instance2 : constant Entry_Spec := Instance.As_Entry_Spec;
         begin
            return
              MP.Match (Pattern2.F_Entry_Name, Instance2.F_Entry_Name)
              and then MP.Match
                (Pattern2.F_Family_Type, Instance2.F_Family_Type)
              and then MP.Match
                (Pattern2.F_Entry_Params, Instance2.F_Entry_Params);
         end;
      elsif Pattern.Kind = Ada_Param_Assoc then
         declare
            Pattern2  : constant Param_Assoc := Pattern.As_Param_Assoc;
            Instance2 : constant Param_Assoc := Instance.As_Param_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Designator, Instance2.F_Designator)
              and then MP.Match (Pattern2.F_R_Expr, Instance2.F_R_Expr);
         end;
      elsif Pattern.Kind = Ada_Generic_Subp_Decl then
         declare
            Pattern2 : constant Generic_Subp_Decl :=
              Pattern.As_Generic_Subp_Decl;
            Instance2 : constant Generic_Subp_Decl :=
              Instance.As_Generic_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Formal_Part, Instance2.F_Formal_Part)
              and then MP.Match (Pattern2.F_Subp_Decl, Instance2.F_Subp_Decl);
         end;
      elsif Pattern.Kind = Ada_Generic_Package_Renaming_Decl then
         declare
            Pattern2 : constant Generic_Package_Renaming_Decl :=
              Pattern.As_Generic_Package_Renaming_Decl;
            Instance2 : constant Generic_Package_Renaming_Decl :=
              Instance.As_Generic_Package_Renaming_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Renames, Instance2.F_Renames);
         end;
      elsif Pattern.Kind = Ada_Unconstrained_Array_Index then
         declare
            Pattern2 : constant Unconstrained_Array_Index :=
              Pattern.As_Unconstrained_Array_Index;
            Instance2 : constant Unconstrained_Array_Index :=
              Instance.As_Unconstrained_Array_Index;
         begin
            return
              MP.Match
                (Pattern2.F_Subtype_Indication,
                 Instance2.F_Subtype_Indication);
         end;
      elsif Pattern.Kind = Ada_Task_Body then
         declare
            Pattern2  : constant Task_Body := Pattern.As_Task_Body;
            Instance2 : constant Task_Body := Instance.As_Task_Body;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Package_Decl then
         declare
            Pattern2  : constant Package_Decl := Pattern.As_Package_Decl;
            Instance2 : constant Package_Decl := Instance.As_Package_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Package_Name, Instance2.F_Package_Name)
              and then MP.Match
                (Pattern2.F_Public_Part, Instance2.F_Public_Part)
              and then MP.Match
                (Pattern2.F_Private_Part, Instance2.F_Private_Part)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Expr_Function then
         declare
            Pattern2  : constant Expr_Function := Pattern.As_Expr_Function;
            Instance2 : constant Expr_Function := Instance.As_Expr_Function;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Component_Clause then
         declare
            Pattern2 : constant Component_Clause :=
              Pattern.As_Component_Clause;
            Instance2 : constant Component_Clause :=
              Instance.As_Component_Clause;
         begin
            return
              MP.Match (Pattern2.F_Id, Instance2.F_Id)
              and then MP.Match (Pattern2.F_Position, Instance2.F_Position)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Elsif_Expr_Part then
         declare
            Pattern2  : constant Elsif_Expr_Part := Pattern.As_Elsif_Expr_Part;
            Instance2 : constant Elsif_Expr_Part :=
              Instance.As_Elsif_Expr_Part;
         begin
            return
              MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr)
              and then MP.Match (Pattern2.F_Then_Expr, Instance2.F_Then_Expr);
         end;
      elsif Pattern.Kind = Ada_Protected_Def then
         declare
            Pattern2  : constant Protected_Def := Pattern.As_Protected_Def;
            Instance2 : constant Protected_Def := Instance.As_Protected_Def;
         begin
            return
              MP.Match (Pattern2.F_Public_Part, Instance2.F_Public_Part)
              and then MP.Match
                (Pattern2.F_Private_Part, Instance2.F_Private_Part)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Raise_Stmt then
         declare
            Pattern2  : constant Raise_Stmt := Pattern.As_Raise_Stmt;
            Instance2 : constant Raise_Stmt := Instance.As_Raise_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Exception_Name, Instance2.F_Exception_Name)
              and then MP.Match
                (Pattern2.F_Error_Message, Instance2.F_Error_Message);
         end;
      elsif Pattern.Kind = Ada_Un_Op then
         declare
            Pattern2  : constant Un_Op := Pattern.As_Un_Op;
            Instance2 : constant Un_Op := Instance.As_Un_Op;
         begin
            return
              MP.Match (Pattern2.F_Op, Instance2.F_Op)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Type_Decl then
         declare
            Pattern2  : constant Type_Decl := Pattern.As_Type_Decl;
            Instance2 : constant Type_Decl := Instance.As_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match (Pattern2.F_Type_Def, Instance2.F_Type_Def);
         end;
      elsif Pattern.Kind = Ada_Null_Record_Aggregate then
         declare
            Pattern2 : constant Null_Record_Aggregate :=
              Pattern.As_Null_Record_Aggregate;
            Instance2 : constant Null_Record_Aggregate :=
              Instance.As_Null_Record_Aggregate;
         begin
            return
              MP.Match (Pattern2.F_Ancestor_Expr, Instance2.F_Ancestor_Expr)
              and then MP.Match (Pattern2.F_Assocs, Instance2.F_Assocs);
         end;
      elsif Pattern.Kind = Ada_For_Loop_Stmt then
         declare
            Pattern2  : constant For_Loop_Stmt := Pattern.As_For_Loop_Stmt;
            Instance2 : constant For_Loop_Stmt := Instance.As_For_Loop_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Spec, Instance2.F_Spec)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Pragma_Argument_Assoc then
         declare
            Pattern2 : constant Pragma_Argument_Assoc :=
              Pattern.As_Pragma_Argument_Assoc;
            Instance2 : constant Pragma_Argument_Assoc :=
              Instance.As_Pragma_Argument_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Qual_Expr then
         declare
            Pattern2  : constant Qual_Expr := Pattern.As_Qual_Expr;
            Instance2 : constant Qual_Expr := Instance.As_Qual_Expr;
         begin
            return
              MP.Match (Pattern2.F_Prefix, Instance2.F_Prefix)
              and then MP.Match (Pattern2.F_Suffix, Instance2.F_Suffix);
         end;
      elsif Pattern.Kind = Ada_Aggregate then
         declare
            Pattern2  : constant Aggregate := Pattern.As_Aggregate;
            Instance2 : constant Aggregate := Instance.As_Aggregate;
         begin
            return
              MP.Match (Pattern2.F_Ancestor_Expr, Instance2.F_Ancestor_Expr)
              and then MP.Match (Pattern2.F_Assocs, Instance2.F_Assocs);
         end;
      elsif Pattern.Kind = Ada_Component_Def then
         declare
            Pattern2  : constant Component_Def := Pattern.As_Component_Def;
            Instance2 : constant Component_Def := Instance.As_Component_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Aliased, Instance2.F_Has_Aliased)
              and then MP.Match
                (Pattern2.F_Has_Constant, Instance2.F_Has_Constant)
              and then MP.Match (Pattern2.F_Type_Expr, Instance2.F_Type_Expr);
         end;
      elsif Pattern.Kind = Ada_Generic_Formal_Part then
         declare
            Pattern2 : constant Generic_Formal_Part :=
              Pattern.As_Generic_Formal_Part;
            Instance2 : constant Generic_Formal_Part :=
              Instance.As_Generic_Formal_Part;
         begin
            return MP.Match (Pattern2.F_Decls, Instance2.F_Decls);
         end;
      elsif Pattern.Kind = Ada_Generic_Formal_Subp_Decl then
         declare
            Pattern2 : constant Generic_Formal_Subp_Decl :=
              Pattern.As_Generic_Formal_Subp_Decl;
            Instance2 : constant Generic_Formal_Subp_Decl :=
              Instance.As_Generic_Formal_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_Aspect_Assoc then
         declare
            Pattern2  : constant Aspect_Assoc := Pattern.As_Aspect_Assoc;
            Instance2 : constant Aspect_Assoc := Instance.As_Aspect_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Id, Instance2.F_Id)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Multi_Dim_Array_Assoc then
         declare
            Pattern2 : constant Multi_Dim_Array_Assoc :=
              Pattern.As_Multi_Dim_Array_Assoc;
            Instance2 : constant Multi_Dim_Array_Assoc :=
              Instance.As_Multi_Dim_Array_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Designators, Instance2.F_Designators)
              and then MP.Match (Pattern2.F_R_Expr, Instance2.F_R_Expr);
         end;
      elsif Pattern.Kind = Ada_Anonymous_Expr_Decl then
         declare
            Pattern2 : constant Anonymous_Expr_Decl :=
              Pattern.As_Anonymous_Expr_Decl;
            Instance2 : constant Anonymous_Expr_Decl :=
              Instance.As_Anonymous_Expr_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Defining_Name then
         declare
            Pattern2  : constant Defining_Name := Pattern.As_Defining_Name;
            Instance2 : constant Defining_Name := Instance.As_Defining_Name;
         begin
            return MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Membership_Expr then
         declare
            Pattern2  : constant Membership_Expr := Pattern.As_Membership_Expr;
            Instance2 : constant Membership_Expr :=
              Instance.As_Membership_Expr;
         begin
            return
              MP.Match (Pattern2.F_Expr, Instance2.F_Expr)
              and then MP.Match (Pattern2.F_Op, Instance2.F_Op)
              and then MP.Match
                (Pattern2.F_Membership_Exprs, Instance2.F_Membership_Exprs);
         end;
      elsif Pattern.Kind = Ada_Single_Protected_Decl then
         declare
            Pattern2 : constant Single_Protected_Decl :=
              Pattern.As_Single_Protected_Decl;
            Instance2 : constant Single_Protected_Decl :=
              Instance.As_Single_Protected_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Interfaces, Instance2.F_Interfaces)
              and then MP.Match
                (Pattern2.F_Definition, Instance2.F_Definition);
         end;
      elsif Pattern.Kind = Ada_Bracket_Delta_Aggregate then
         declare
            Pattern2 : constant Bracket_Delta_Aggregate :=
              Pattern.As_Bracket_Delta_Aggregate;
            Instance2 : constant Bracket_Delta_Aggregate :=
              Instance.As_Bracket_Delta_Aggregate;
         begin
            return
              MP.Match (Pattern2.F_Ancestor_Expr, Instance2.F_Ancestor_Expr)
              and then MP.Match (Pattern2.F_Assocs, Instance2.F_Assocs);
         end;
      elsif Pattern.Kind = Ada_Digits_Constraint then
         declare
            Pattern2 : constant Digits_Constraint :=
              Pattern.As_Digits_Constraint;
            Instance2 : constant Digits_Constraint :=
              Instance.As_Digits_Constraint;
         begin
            return
              MP.Match (Pattern2.F_Digits, Instance2.F_Digits)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Paren_Expr then
         declare
            Pattern2  : constant Paren_Expr := Pattern.As_Paren_Expr;
            Instance2 : constant Paren_Expr := Instance.As_Paren_Expr;
         begin
            return MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Component_Decl then
         declare
            Pattern2  : constant Component_Decl := Pattern.As_Component_Decl;
            Instance2 : constant Component_Decl := Instance.As_Component_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match
                (Pattern2.F_Component_Def, Instance2.F_Component_Def)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr);
         end;
      elsif Pattern.Kind = Ada_Case_Stmt then
         declare
            Pattern2  : constant Case_Stmt := Pattern.As_Case_Stmt;
            Instance2 : constant Case_Stmt := Instance.As_Case_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Expr, Instance2.F_Expr)
              and then MP.Match
                (Pattern2.F_Alternatives, Instance2.F_Alternatives);
         end;
      elsif Pattern.Kind = Ada_Use_Type_Clause then
         declare
            Pattern2  : constant Use_Type_Clause := Pattern.As_Use_Type_Clause;
            Instance2 : constant Use_Type_Clause :=
              Instance.As_Use_Type_Clause;
         begin
            return
              MP.Match (Pattern2.F_Has_All, Instance2.F_Has_All)
              and then MP.Match (Pattern2.F_Types, Instance2.F_Types);
         end;
      elsif Pattern.Kind = Ada_Discriminant_Spec then
         declare
            Pattern2 : constant Discriminant_Spec :=
              Pattern.As_Discriminant_Spec;
            Instance2 : constant Discriminant_Spec :=
              Instance.As_Discriminant_Spec;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match (Pattern2.F_Type_Expr, Instance2.F_Type_Expr)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr);
         end;
      elsif Pattern.Kind = Ada_Number_Decl then
         declare
            Pattern2  : constant Number_Decl := Pattern.As_Number_Decl;
            Instance2 : constant Number_Decl := Instance.As_Number_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Synthetic_Renaming_Clause then
         declare
            Pattern2 : constant Synthetic_Renaming_Clause :=
              Pattern.As_Synthetic_Renaming_Clause;
            Instance2 : constant Synthetic_Renaming_Clause :=
              Instance.As_Synthetic_Renaming_Clause;
         begin
            return
              MP.Match (Pattern2.F_Renamed_Object, Instance2.F_Renamed_Object);
         end;
      elsif Pattern.Kind = Ada_Enum_Literal_Decl then
         declare
            Pattern2 : constant Enum_Literal_Decl :=
              Pattern.As_Enum_Literal_Decl;
            Instance2 : constant Enum_Literal_Decl :=
              Instance.As_Enum_Literal_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Signed_Int_Type_Def then
         declare
            Pattern2 : constant Signed_Int_Type_Def :=
              Pattern.As_Signed_Int_Type_Def;
            Instance2 : constant Signed_Int_Type_Def :=
              Instance.As_Signed_Int_Type_Def;
         begin
            return MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Attribute_Def_Clause then
         declare
            Pattern2 : constant Attribute_Def_Clause :=
              Pattern.As_Attribute_Def_Clause;
            Instance2 : constant Attribute_Def_Clause :=
              Instance.As_Attribute_Def_Clause;
         begin
            return
              MP.Match (Pattern2.F_Attribute_Expr, Instance2.F_Attribute_Expr)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Compilation_Unit then
         declare
            Pattern2 : constant Compilation_Unit :=
              Pattern.As_Compilation_Unit;
            Instance2 : constant Compilation_Unit :=
              Instance.As_Compilation_Unit;
         begin
            return
              MP.Match (Pattern2.F_Prelude, Instance2.F_Prelude)
              and then MP.Match (Pattern2.F_Body, Instance2.F_Body)
              and then MP.Match (Pattern2.F_Pragmas, Instance2.F_Pragmas);
         end;
      elsif Pattern.Kind = Ada_Param_Spec then
         declare
            Pattern2  : constant Param_Spec := Pattern.As_Param_Spec;
            Instance2 : constant Param_Spec := Instance.As_Param_Spec;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match
                (Pattern2.F_Has_Aliased, Instance2.F_Has_Aliased)
              and then MP.Match (Pattern2.F_Mode, Instance2.F_Mode)
              and then MP.Match (Pattern2.F_Type_Expr, Instance2.F_Type_Expr)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr);
         end;
      elsif Pattern.Kind = Ada_Constrained_Array_Indices then
         declare
            Pattern2 : constant Constrained_Array_Indices :=
              Pattern.As_Constrained_Array_Indices;
            Instance2 : constant Constrained_Array_Indices :=
              Instance.As_Constrained_Array_Indices;
         begin
            return MP.Match (Pattern2.F_List, Instance2.F_List);
         end;
      elsif Pattern.Kind = Ada_Entry_Index_Spec then
         declare
            Pattern2 : constant Entry_Index_Spec :=
              Pattern.As_Entry_Index_Spec;
            Instance2 : constant Entry_Index_Spec :=
              Instance.As_Entry_Index_Spec;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Id, Instance2.F_Id)
              and then MP.Match (Pattern2.F_Subtype, Instance2.F_Subtype);
         end;
      elsif Pattern.Kind = Ada_Multi_Abstract_State_Decl then
         declare
            Pattern2 : constant Multi_Abstract_State_Decl :=
              Pattern.As_Multi_Abstract_State_Decl;
            Instance2 : constant Multi_Abstract_State_Decl :=
              Instance.As_Multi_Abstract_State_Decl;
         begin
            return MP.Match (Pattern2.F_Decls, Instance2.F_Decls);
         end;
      elsif Pattern.Kind = Ada_Renaming_Clause then
         declare
            Pattern2  : constant Renaming_Clause := Pattern.As_Renaming_Clause;
            Instance2 : constant Renaming_Clause :=
              Instance.As_Renaming_Clause;
         begin
            return
              MP.Match (Pattern2.F_Renamed_Object, Instance2.F_Renamed_Object);
         end;
      elsif Pattern.Kind = Ada_Private_Type_Def then
         declare
            Pattern2 : constant Private_Type_Def :=
              Pattern.As_Private_Type_Def;
            Instance2 : constant Private_Type_Def :=
              Instance.As_Private_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Abstract, Instance2.F_Has_Abstract)
              and then MP.Match (Pattern2.F_Has_Tagged, Instance2.F_Has_Tagged)
              and then MP.Match
                (Pattern2.F_Has_Limited, Instance2.F_Has_Limited);
         end;
      elsif Pattern.Kind = Ada_If_Expr then
         declare
            Pattern2  : constant If_Expr := Pattern.As_If_Expr;
            Instance2 : constant If_Expr := Instance.As_If_Expr;
         begin
            return
              MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr)
              and then MP.Match (Pattern2.F_Then_Expr, Instance2.F_Then_Expr)
              and then MP.Match
                (Pattern2.F_Alternatives, Instance2.F_Alternatives)
              and then MP.Match (Pattern2.F_Else_Expr, Instance2.F_Else_Expr);
         end;
      elsif Pattern.Kind = Ada_Declarative_Part then
         declare
            Pattern2 : constant Declarative_Part :=
              Pattern.As_Declarative_Part;
            Instance2 : constant Declarative_Part :=
              Instance.As_Declarative_Part;
         begin
            return MP.Match (Pattern2.F_Decls, Instance2.F_Decls);
         end;
      elsif Pattern.Kind = Ada_Handled_Stmts then
         declare
            Pattern2  : constant Handled_Stmts := Pattern.As_Handled_Stmts;
            Instance2 : constant Handled_Stmts := Instance.As_Handled_Stmts;
         begin
            return
              MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match
                (Pattern2.F_Exceptions, Instance2.F_Exceptions);
         end;
      elsif Pattern.Kind = Ada_Params then
         declare
            Pattern2  : constant Params := Pattern.As_Params;
            Instance2 : constant Params := Instance.As_Params;
         begin
            return MP.Match (Pattern2.F_Params, Instance2.F_Params);
         end;
      elsif Pattern.Kind = Ada_Loop_Stmt then
         declare
            Pattern2  : constant Loop_Stmt := Pattern.As_Loop_Stmt;
            Instance2 : constant Loop_Stmt := Instance.As_Loop_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Spec, Instance2.F_Spec)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Subtype_Indication then
         declare
            Pattern2 : constant Subtype_Indication :=
              Pattern.As_Subtype_Indication;
            Instance2 : constant Subtype_Indication :=
              Instance.As_Subtype_Indication;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Constraint, Instance2.F_Constraint);
         end;
      elsif Pattern.Kind = Ada_Classwide_Type_Decl then
         declare
            Pattern2 : constant Classwide_Type_Decl :=
              Pattern.As_Classwide_Type_Decl;
            Instance2 : constant Classwide_Type_Decl :=
              Instance.As_Classwide_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Exit_Stmt then
         declare
            Pattern2  : constant Exit_Stmt := Pattern.As_Exit_Stmt;
            Instance2 : constant Exit_Stmt := Instance.As_Exit_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Loop_Name, Instance2.F_Loop_Name)
              and then MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr);
         end;
      elsif Pattern.Kind = Ada_Enum_Rep_Clause then
         declare
            Pattern2  : constant Enum_Rep_Clause := Pattern.As_Enum_Rep_Clause;
            Instance2 : constant Enum_Rep_Clause :=
              Instance.As_Enum_Rep_Clause;
         begin
            return
              MP.Match (Pattern2.F_Type_Name, Instance2.F_Type_Name)
              and then MP.Match (Pattern2.F_Aggregate, Instance2.F_Aggregate);
         end;
      elsif Pattern.Kind = Ada_Derived_Type_Def then
         declare
            Pattern2 : constant Derived_Type_Def :=
              Pattern.As_Derived_Type_Def;
            Instance2 : constant Derived_Type_Def :=
              Instance.As_Derived_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Abstract, Instance2.F_Has_Abstract)
              and then MP.Match
                (Pattern2.F_Has_Limited, Instance2.F_Has_Limited)
              and then MP.Match
                (Pattern2.F_Has_Synchronized, Instance2.F_Has_Synchronized)
              and then MP.Match
                (Pattern2.F_Subtype_Indication, Instance2.F_Subtype_Indication)
              and then MP.Match (Pattern2.F_Interfaces, Instance2.F_Interfaces)
              and then MP.Match
                (Pattern2.F_Record_Extension, Instance2.F_Record_Extension)
              and then MP.Match
                (Pattern2.F_Has_With_Private, Instance2.F_Has_With_Private);
         end;
      elsif Pattern.Kind = Ada_Accept_Stmt_With_Stmts then
         declare
            Pattern2 : constant Accept_Stmt_With_Stmts :=
              Pattern.As_Accept_Stmt_With_Stmts;
            Instance2 : constant Accept_Stmt_With_Stmts :=
              Instance.As_Accept_Stmt_With_Stmts;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Entry_Index_Expr, Instance2.F_Entry_Index_Expr)
              and then MP.Match (Pattern2.F_Params, Instance2.F_Params)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Call_Stmt then
         declare
            Pattern2  : constant Call_Stmt := Pattern.As_Call_Stmt;
            Instance2 : constant Call_Stmt := Instance.As_Call_Stmt;
         begin
            return MP.Match (Pattern2.F_Call, Instance2.F_Call);
         end;
      elsif Pattern.Kind = Ada_Discriminant_Assoc then
         declare
            Pattern2 : constant Discriminant_Assoc :=
              Pattern.As_Discriminant_Assoc;
            Instance2 : constant Discriminant_Assoc :=
              Instance.As_Discriminant_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match
                (Pattern2.F_Discr_Expr, Instance2.F_Discr_Expr);
         end;
      elsif Pattern.Kind = Ada_Array_Type_Def then
         declare
            Pattern2  : constant Array_Type_Def := Pattern.As_Array_Type_Def;
            Instance2 : constant Array_Type_Def := Instance.As_Array_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Indices, Instance2.F_Indices)
              and then MP.Match
                (Pattern2.F_Component_Type, Instance2.F_Component_Type);
         end;
      elsif Pattern.Kind = Ada_Label then
         declare
            Pattern2  : constant Label := Pattern.As_Label;
            Instance2 : constant Label := Instance.As_Label;
         begin
            return MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_For_Loop_Var_Decl then
         declare
            Pattern2 : constant For_Loop_Var_Decl :=
              Pattern.As_For_Loop_Var_Decl;
            Instance2 : constant For_Loop_Var_Decl :=
              Instance.As_For_Loop_Var_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Id, Instance2.F_Id)
              and then MP.Match (Pattern2.F_Id_Type, Instance2.F_Id_Type);
         end;
      elsif Pattern.Kind = Ada_Dotted_Name then
         declare
            Pattern2  : constant Dotted_Name := Pattern.As_Dotted_Name;
            Instance2 : constant Dotted_Name := Instance.As_Dotted_Name;
         begin
            return
              MP.Match (Pattern2.F_Prefix, Instance2.F_Prefix)
              and then MP.Match (Pattern2.F_Suffix, Instance2.F_Suffix);
         end;
      elsif Pattern.Kind = Ada_Known_Discriminant_Part then
         declare
            Pattern2 : constant Known_Discriminant_Part :=
              Pattern.As_Known_Discriminant_Part;
            Instance2 : constant Known_Discriminant_Part :=
              Instance.As_Known_Discriminant_Part;
         begin
            return MP.Match (Pattern2.F_Discr_Specs, Instance2.F_Discr_Specs);
         end;
      elsif Pattern.Kind = Ada_Index_Constraint then
         declare
            Pattern2 : constant Index_Constraint :=
              Pattern.As_Index_Constraint;
            Instance2 : constant Index_Constraint :=
              Instance.As_Index_Constraint;
         begin
            return MP.Match (Pattern2.F_Constraints, Instance2.F_Constraints);
         end;
      elsif Pattern.Kind = Ada_Discrete_Base_Subtype_Decl then
         declare
            Pattern2 : constant Discrete_Base_Subtype_Decl :=
              Pattern.As_Discrete_Base_Subtype_Decl;
            Instance2 : constant Discrete_Base_Subtype_Decl :=
              Instance.As_Discrete_Base_Subtype_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Protected_Body_Stub then
         declare
            Pattern2 : constant Protected_Body_Stub :=
              Pattern.As_Protected_Body_Stub;
            Instance2 : constant Protected_Body_Stub :=
              Instance.As_Protected_Body_Stub;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Package_Renaming_Decl then
         declare
            Pattern2 : constant Package_Renaming_Decl :=
              Pattern.As_Package_Renaming_Decl;
            Instance2 : constant Package_Renaming_Decl :=
              Instance.As_Package_Renaming_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Renames, Instance2.F_Renames);
         end;
      elsif Pattern.Kind = Ada_Elsif_Stmt_Part then
         declare
            Pattern2  : constant Elsif_Stmt_Part := Pattern.As_Elsif_Stmt_Part;
            Instance2 : constant Elsif_Stmt_Part :=
              Instance.As_Elsif_Stmt_Part;
         begin
            return
              MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts);
         end;
      elsif Pattern.Kind = Ada_Record_Type_Def then
         declare
            Pattern2  : constant Record_Type_Def := Pattern.As_Record_Type_Def;
            Instance2 : constant Record_Type_Def :=
              Instance.As_Record_Type_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Abstract, Instance2.F_Has_Abstract)
              and then MP.Match (Pattern2.F_Has_Tagged, Instance2.F_Has_Tagged)
              and then MP.Match
                (Pattern2.F_Has_Limited, Instance2.F_Has_Limited)
              and then MP.Match
                (Pattern2.F_Record_Def, Instance2.F_Record_Def);
         end;
      elsif Pattern.Kind = Ada_Delay_Stmt then
         declare
            Pattern2  : constant Delay_Stmt := Pattern.As_Delay_Stmt;
            Instance2 : constant Delay_Stmt := Instance.As_Delay_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Has_Until, Instance2.F_Has_Until)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Subunit then
         declare
            Pattern2  : constant Subunit := Pattern.As_Subunit;
            Instance2 : constant Subunit := Instance.As_Subunit;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Body, Instance2.F_Body);
         end;
      elsif Pattern.Kind = Ada_Bracket_Aggregate then
         declare
            Pattern2 : constant Bracket_Aggregate :=
              Pattern.As_Bracket_Aggregate;
            Instance2 : constant Bracket_Aggregate :=
              Instance.As_Bracket_Aggregate;
         begin
            return
              MP.Match (Pattern2.F_Ancestor_Expr, Instance2.F_Ancestor_Expr)
              and then MP.Match (Pattern2.F_Assocs, Instance2.F_Assocs);
         end;
      elsif Pattern.Kind = Ada_Return_Stmt then
         declare
            Pattern2  : constant Return_Stmt := Pattern.As_Return_Stmt;
            Instance2 : constant Return_Stmt := Instance.As_Return_Stmt;
         begin
            return MP.Match (Pattern2.F_Return_Expr, Instance2.F_Return_Expr);
         end;
      elsif Pattern.Kind = Ada_Exception_Handler then
         declare
            Pattern2 : constant Exception_Handler :=
              Pattern.As_Exception_Handler;
            Instance2 : constant Exception_Handler :=
              Instance.As_Exception_Handler;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Exception_Name, Instance2.F_Exception_Name)
              and then MP.Match
                (Pattern2.F_Handled_Exceptions, Instance2.F_Handled_Exceptions)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts);
         end;
      elsif Pattern.Kind = Ada_Update_Attribute_Ref then
         declare
            Pattern2 : constant Update_Attribute_Ref :=
              Pattern.As_Update_Attribute_Ref;
            Instance2 : constant Update_Attribute_Ref :=
              Instance.As_Update_Attribute_Ref;
         begin
            return
              MP.Match (Pattern2.F_Prefix, Instance2.F_Prefix)
              and then MP.Match (Pattern2.F_Attribute, Instance2.F_Attribute)
              and then MP.Match (Pattern2.F_Args, Instance2.F_Args);
         end;
      elsif Pattern.Kind = Ada_Record_Def then
         declare
            Pattern2  : constant Record_Def := Pattern.As_Record_Def;
            Instance2 : constant Record_Def := Instance.As_Record_Def;
         begin
            return MP.Match (Pattern2.F_Components, Instance2.F_Components);
         end;
      elsif Pattern.Kind = Ada_Null_Record_Def then
         declare
            Pattern2  : constant Null_Record_Def := Pattern.As_Null_Record_Def;
            Instance2 : constant Null_Record_Def :=
              Instance.As_Null_Record_Def;
         begin
            return MP.Match (Pattern2.F_Components, Instance2.F_Components);
         end;
      elsif Pattern.Kind = Ada_Generic_Subp_Renaming_Decl then
         declare
            Pattern2 : constant Generic_Subp_Renaming_Decl :=
              Pattern.As_Generic_Subp_Renaming_Decl;
            Instance2 : constant Generic_Subp_Renaming_Decl :=
              Instance.As_Generic_Subp_Renaming_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Kind, Instance2.F_Kind)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Renames, Instance2.F_Renames);
         end;
      elsif Pattern.Kind = Ada_Bin_Op then
         declare
            Pattern2  : constant Bin_Op := Pattern.As_Bin_Op;
            Instance2 : constant Bin_Op := Instance.As_Bin_Op;
         begin
            return
              MP.Match (Pattern2.F_Left, Instance2.F_Left)
              and then MP.Match (Pattern2.F_Op, Instance2.F_Op)
              and then MP.Match (Pattern2.F_Right, Instance2.F_Right);
         end;
      elsif Pattern.Kind = Ada_Abstract_State_Decl then
         declare
            Pattern2 : constant Abstract_State_Decl :=
              Pattern.As_Abstract_State_Decl;
            Instance2 : constant Abstract_State_Decl :=
              Instance.As_Abstract_State_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Record_Rep_Clause then
         declare
            Pattern2 : constant Record_Rep_Clause :=
              Pattern.As_Record_Rep_Clause;
            Instance2 : constant Record_Rep_Clause :=
              Instance.As_Record_Rep_Clause;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_At_Expr, Instance2.F_At_Expr)
              and then MP.Match
                (Pattern2.F_Components, Instance2.F_Components);
         end;
      elsif Pattern.Kind = Ada_Range_Spec then
         declare
            Pattern2  : constant Range_Spec := Pattern.As_Range_Spec;
            Instance2 : constant Range_Spec := Instance.As_Range_Spec;
         begin
            return MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_With_Clause then
         declare
            Pattern2  : constant With_Clause := Pattern.As_With_Clause;
            Instance2 : constant With_Clause := Instance.As_With_Clause;
         begin
            return
              MP.Match (Pattern2.F_Has_Limited, Instance2.F_Has_Limited)
              and then MP.Match
                (Pattern2.F_Has_Private, Instance2.F_Has_Private)
              and then MP.Match (Pattern2.F_Packages, Instance2.F_Packages);
         end;
      elsif Pattern.Kind = Ada_Subp_Body then
         declare
            Pattern2  : constant Subp_Body := Pattern.As_Subp_Body;
            Instance2 : constant Subp_Body := Instance.As_Subp_Body;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec)
              and then MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Private_Part then
         declare
            Pattern2  : constant Private_Part := Pattern.As_Private_Part;
            Instance2 : constant Private_Part := Instance.As_Private_Part;
         begin
            return MP.Match (Pattern2.F_Decls, Instance2.F_Decls);
         end;
      elsif Pattern.Kind = Ada_Abstract_State_Decl_Expr then
         declare
            Pattern2 : constant Abstract_State_Decl_Expr :=
              Pattern.As_Abstract_State_Decl_Expr;
            Instance2 : constant Abstract_State_Decl_Expr :=
              Instance.As_Abstract_State_Decl_Expr;
         begin
            return MP.Match (Pattern2.F_State_Decl, Instance2.F_State_Decl);
         end;
      elsif Pattern.Kind = Ada_Select_Stmt then
         declare
            Pattern2  : constant Select_Stmt := Pattern.As_Select_Stmt;
            Instance2 : constant Select_Stmt := Instance.As_Select_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Guards, Instance2.F_Guards)
              and then MP.Match (Pattern2.F_Else_Stmts, Instance2.F_Else_Stmts)
              and then MP.Match
                (Pattern2.F_Abort_Stmts, Instance2.F_Abort_Stmts);
         end;
      elsif Pattern.Kind = Ada_Subp_Renaming_Decl then
         declare
            Pattern2 : constant Subp_Renaming_Decl :=
              Pattern.As_Subp_Renaming_Decl;
            Instance2 : constant Subp_Renaming_Decl :=
              Instance.As_Subp_Renaming_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec)
              and then MP.Match (Pattern2.F_Renames, Instance2.F_Renames);
         end;
      elsif Pattern.Kind = Ada_Generic_Formal_Obj_Decl then
         declare
            Pattern2 : constant Generic_Formal_Obj_Decl :=
              Pattern.As_Generic_Formal_Obj_Decl;
            Instance2 : constant Generic_Formal_Obj_Decl :=
              Instance.As_Generic_Formal_Obj_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_Discrete_Subtype_Indication then
         declare
            Pattern2 : constant Discrete_Subtype_Indication :=
              Pattern.As_Discrete_Subtype_Indication;
            Instance2 : constant Discrete_Subtype_Indication :=
              Instance.As_Discrete_Subtype_Indication;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Constraint, Instance2.F_Constraint);
         end;
      elsif Pattern.Kind = Ada_Package_Body_Stub then
         declare
            Pattern2 : constant Package_Body_Stub :=
              Pattern.As_Package_Body_Stub;
            Instance2 : constant Package_Body_Stub :=
              Instance.As_Package_Body_Stub;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Requeue_Stmt then
         declare
            Pattern2  : constant Requeue_Stmt := Pattern.As_Requeue_Stmt;
            Instance2 : constant Requeue_Stmt := Instance.As_Requeue_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Call_Name, Instance2.F_Call_Name)
              and then MP.Match (Pattern2.F_Has_Abort, Instance2.F_Has_Abort);
         end;
      elsif Pattern.Kind = Ada_Accept_Stmt then
         declare
            Pattern2  : constant Accept_Stmt := Pattern.As_Accept_Stmt;
            Instance2 : constant Accept_Stmt := Instance.As_Accept_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Entry_Index_Expr, Instance2.F_Entry_Index_Expr)
              and then MP.Match (Pattern2.F_Params, Instance2.F_Params);
         end;
      elsif Pattern.Kind = Ada_Task_Type_Decl then
         declare
            Pattern2  : constant Task_Type_Decl := Pattern.As_Task_Type_Decl;
            Instance2 : constant Task_Type_Decl := Instance.As_Task_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match
                (Pattern2.F_Definition, Instance2.F_Definition);
         end;
      elsif Pattern.Kind = Ada_Contract_Cases then
         declare
            Pattern2  : constant Contract_Cases := Pattern.As_Contract_Cases;
            Instance2 : constant Contract_Cases := Instance.As_Contract_Cases;
         begin
            return
              MP.Match (Pattern2.F_Contract_Cases, Instance2.F_Contract_Cases);
         end;
      elsif Pattern.Kind = Ada_Decl_Block then
         declare
            Pattern2  : constant Decl_Block := Pattern.As_Decl_Block;
            Instance2 : constant Decl_Block := Instance.As_Decl_Block;
         begin
            return
              MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Constrained_Subtype_Indication then
         declare
            Pattern2 : constant Constrained_Subtype_Indication :=
              Pattern.As_Constrained_Subtype_Indication;
            Instance2 : constant Constrained_Subtype_Indication :=
              Instance.As_Constrained_Subtype_Indication;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Constraint, Instance2.F_Constraint);
         end;
      elsif Pattern.Kind = Ada_Floating_Point_Def then
         declare
            Pattern2 : constant Floating_Point_Def :=
              Pattern.As_Floating_Point_Def;
            Instance2 : constant Floating_Point_Def :=
              Instance.As_Floating_Point_Def;
         begin
            return
              MP.Match (Pattern2.F_Num_Digits, Instance2.F_Num_Digits)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Entry_Body then
         declare
            Pattern2  : constant Entry_Body := Pattern.As_Entry_Body;
            Instance2 : constant Entry_Body := Instance.As_Entry_Body;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Entry_Name, Instance2.F_Entry_Name)
              and then MP.Match (Pattern2.F_Index_Spec, Instance2.F_Index_Spec)
              and then MP.Match (Pattern2.F_Params, Instance2.F_Params)
              and then MP.Match (Pattern2.F_Barrier, Instance2.F_Barrier)
              and then MP.Match (Pattern2.F_Decls, Instance2.F_Decls)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Attribute_Ref then
         declare
            Pattern2  : constant Attribute_Ref := Pattern.As_Attribute_Ref;
            Instance2 : constant Attribute_Ref := Instance.As_Attribute_Ref;
         begin
            return
              MP.Match (Pattern2.F_Prefix, Instance2.F_Prefix)
              and then MP.Match (Pattern2.F_Attribute, Instance2.F_Attribute)
              and then MP.Match (Pattern2.F_Args, Instance2.F_Args);
         end;
      elsif Pattern.Kind = Ada_Unconstrained_Array_Indices then
         declare
            Pattern2 : constant Unconstrained_Array_Indices :=
              Pattern.As_Unconstrained_Array_Indices;
            Instance2 : constant Unconstrained_Array_Indices :=
              Instance.As_Unconstrained_Array_Indices;
         begin
            return MP.Match (Pattern2.F_Types, Instance2.F_Types);
         end;
      elsif Pattern.Kind = Ada_Extended_Return_Stmt then
         declare
            Pattern2 : constant Extended_Return_Stmt :=
              Pattern.As_Extended_Return_Stmt;
            Instance2 : constant Extended_Return_Stmt :=
              Instance.As_Extended_Return_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Decl, Instance2.F_Decl)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts);
         end;
      elsif Pattern.Kind = Ada_Mod_Int_Type_Def then
         declare
            Pattern2 : constant Mod_Int_Type_Def :=
              Pattern.As_Mod_Int_Type_Def;
            Instance2 : constant Mod_Int_Type_Def :=
              Instance.As_Mod_Int_Type_Def;
         begin
            return MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Select_When_Part then
         declare
            Pattern2 : constant Select_When_Part :=
              Pattern.As_Select_When_Part;
            Instance2 : constant Select_When_Part :=
              Instance.As_Select_When_Part;
         begin
            return
              MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr)
              and then MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts);
         end;
      elsif Pattern.Kind = Ada_Explicit_Deref then
         declare
            Pattern2  : constant Explicit_Deref := Pattern.As_Explicit_Deref;
            Instance2 : constant Explicit_Deref := Instance.As_Explicit_Deref;
         begin
            return MP.Match (Pattern2.F_Prefix, Instance2.F_Prefix);
         end;
      elsif Pattern.Kind = Ada_Entry_Completion_Formal_Params then
         declare
            Pattern2 : constant Entry_Completion_Formal_Params :=
              Pattern.As_Entry_Completion_Formal_Params;
            Instance2 : constant Entry_Completion_Formal_Params :=
              Instance.As_Entry_Completion_Formal_Params;
         begin
            return MP.Match (Pattern2.F_Params, Instance2.F_Params);
         end;
      elsif Pattern.Kind = Ada_Generic_Package_Decl then
         declare
            Pattern2 : constant Generic_Package_Decl :=
              Pattern.As_Generic_Package_Decl;
            Instance2 : constant Generic_Package_Decl :=
              Instance.As_Generic_Package_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Formal_Part, Instance2.F_Formal_Part)
              and then MP.Match
                (Pattern2.F_Package_Decl, Instance2.F_Package_Decl);
         end;
      elsif Pattern.Kind = Ada_Subp_Decl then
         declare
            Pattern2  : constant Subp_Decl := Pattern.As_Subp_Decl;
            Instance2 : constant Subp_Decl := Instance.As_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Named_Stmt then
         declare
            Pattern2  : constant Named_Stmt := Pattern.As_Named_Stmt;
            Instance2 : constant Named_Stmt := Instance.As_Named_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Decl, Instance2.F_Decl)
              and then MP.Match (Pattern2.F_Stmt, Instance2.F_Stmt);
         end;
      elsif Pattern.Kind = Ada_At_Clause then
         declare
            Pattern2  : constant At_Clause := Pattern.As_At_Clause;
            Instance2 : constant At_Clause := Instance.As_At_Clause;
         begin
            return
              MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Entry_Decl then
         declare
            Pattern2  : constant Entry_Decl := Pattern.As_Entry_Decl;
            Instance2 : constant Entry_Decl := Instance.As_Entry_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Spec, Instance2.F_Spec);
         end;
      elsif Pattern.Kind = Ada_Concrete_Formal_Subp_Decl then
         declare
            Pattern2 : constant Concrete_Formal_Subp_Decl :=
              Pattern.As_Concrete_Formal_Subp_Decl;
            Instance2 : constant Concrete_Formal_Subp_Decl :=
              Instance.As_Concrete_Formal_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr);
         end;
      elsif Pattern.Kind = Ada_Single_Task_Type_Decl then
         declare
            Pattern2 : constant Single_Task_Type_Decl :=
              Pattern.As_Single_Task_Type_Decl;
            Instance2 : constant Single_Task_Type_Decl :=
              Instance.As_Single_Task_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match
                (Pattern2.F_Definition, Instance2.F_Definition);
         end;
      elsif Pattern.Kind = Ada_Quantified_Expr then
         declare
            Pattern2  : constant Quantified_Expr := Pattern.As_Quantified_Expr;
            Instance2 : constant Quantified_Expr :=
              Instance.As_Quantified_Expr;
         begin
            return
              MP.Match (Pattern2.F_Quantifier, Instance2.F_Quantifier)
              and then MP.Match (Pattern2.F_Loop_Spec, Instance2.F_Loop_Spec)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Pragma_Node then
         declare
            Pattern2  : constant Pragma_Node := Pattern.As_Pragma_Node;
            Instance2 : constant Pragma_Node := Instance.As_Pragma_Node;
         begin
            return
              MP.Match (Pattern2.F_Id, Instance2.F_Id)
              and then MP.Match (Pattern2.F_Args, Instance2.F_Args);
         end;
      elsif Pattern.Kind = Ada_Abort_Stmt then
         declare
            Pattern2  : constant Abort_Stmt := Pattern.As_Abort_Stmt;
            Instance2 : constant Abort_Stmt := Instance.As_Abort_Stmt;
         begin
            return MP.Match (Pattern2.F_Names, Instance2.F_Names);
         end;
      elsif Pattern.Kind = Ada_Generic_Package_Instantiation then
         declare
            Pattern2 : constant Generic_Package_Instantiation :=
              Pattern.As_Generic_Package_Instantiation;
            Instance2 : constant Generic_Package_Instantiation :=
              Instance.As_Generic_Package_Instantiation;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Generic_Pkg_Name, Instance2.F_Generic_Pkg_Name)
              and then MP.Match (Pattern2.F_Params, Instance2.F_Params);
         end;
      elsif Pattern.Kind = Ada_If_Stmt then
         declare
            Pattern2  : constant If_Stmt := Pattern.As_If_Stmt;
            Instance2 : constant If_Stmt := Instance.As_If_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Cond_Expr, Instance2.F_Cond_Expr)
              and then MP.Match (Pattern2.F_Then_Stmts, Instance2.F_Then_Stmts)
              and then MP.Match
                (Pattern2.F_Alternatives, Instance2.F_Alternatives)
              and then MP.Match
                (Pattern2.F_Else_Stmts, Instance2.F_Else_Stmts);
         end;
      elsif Pattern.Kind = Ada_Variant then
         declare
            Pattern2  : constant Variant := Pattern.As_Variant;
            Instance2 : constant Variant := Instance.As_Variant;
         begin
            return
              MP.Match (Pattern2.F_Choices, Instance2.F_Choices)
              and then MP.Match
                (Pattern2.F_Components, Instance2.F_Components);
         end;
      elsif Pattern.Kind = Ada_Object_Decl then
         declare
            Pattern2  : constant Object_Decl := Pattern.As_Object_Decl;
            Instance2 : constant Object_Decl := Instance.As_Object_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match
                (Pattern2.F_Has_Aliased, Instance2.F_Has_Aliased)
              and then MP.Match
                (Pattern2.F_Has_Constant, Instance2.F_Has_Constant)
              and then MP.Match (Pattern2.F_Mode, Instance2.F_Mode)
              and then MP.Match (Pattern2.F_Type_Expr, Instance2.F_Type_Expr)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr)
              and then MP.Match
                (Pattern2.F_Renaming_Clause, Instance2.F_Renaming_Clause);
         end;
      elsif Pattern.Kind = Ada_Goto_Stmt then
         declare
            Pattern2  : constant Goto_Stmt := Pattern.As_Goto_Stmt;
            Instance2 : constant Goto_Stmt := Instance.As_Goto_Stmt;
         begin
            return MP.Match (Pattern2.F_Label_Name, Instance2.F_Label_Name);
         end;
      elsif Pattern.Kind = Ada_While_Loop_Spec then
         declare
            Pattern2  : constant While_Loop_Spec := Pattern.As_While_Loop_Spec;
            Instance2 : constant While_Loop_Spec :=
              Instance.As_While_Loop_Spec;
         begin
            return MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Access_To_Subp_Def then
         declare
            Pattern2 : constant Access_To_Subp_Def :=
              Pattern.As_Access_To_Subp_Def;
            Instance2 : constant Access_To_Subp_Def :=
              Instance.As_Access_To_Subp_Def;
         begin
            return
              MP.Match (Pattern2.F_Has_Not_Null, Instance2.F_Has_Not_Null)
              and then MP.Match
                (Pattern2.F_Has_Protected, Instance2.F_Has_Protected)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Task_Def then
         declare
            Pattern2  : constant Task_Def := Pattern.As_Task_Def;
            Instance2 : constant Task_Def := Instance.As_Task_Def;
         begin
            return
              MP.Match (Pattern2.F_Interfaces, Instance2.F_Interfaces)
              and then MP.Match
                (Pattern2.F_Public_Part, Instance2.F_Public_Part)
              and then MP.Match
                (Pattern2.F_Private_Part, Instance2.F_Private_Part)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Null_Subp_Decl then
         declare
            Pattern2  : constant Null_Subp_Decl := Pattern.As_Null_Subp_Decl;
            Instance2 : constant Null_Subp_Decl := Instance.As_Null_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Relation_Op then
         declare
            Pattern2  : constant Relation_Op := Pattern.As_Relation_Op;
            Instance2 : constant Relation_Op := Instance.As_Relation_Op;
         begin
            return
              MP.Match (Pattern2.F_Left, Instance2.F_Left)
              and then MP.Match (Pattern2.F_Op, Instance2.F_Op)
              and then MP.Match (Pattern2.F_Right, Instance2.F_Right);
         end;
      elsif Pattern.Kind = Ada_Subp_Body_Stub then
         declare
            Pattern2  : constant Subp_Body_Stub := Pattern.As_Subp_Body_Stub;
            Instance2 : constant Subp_Body_Stub := Instance.As_Subp_Body_Stub;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Generic_Formal_Type_Decl then
         declare
            Pattern2 : constant Generic_Formal_Type_Decl :=
              Pattern.As_Generic_Formal_Type_Decl;
            Instance2 : constant Generic_Formal_Type_Decl :=
              Instance.As_Generic_Formal_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Decl, Instance2.F_Decl);
         end;
      elsif Pattern.Kind = Ada_Component_List then
         declare
            Pattern2  : constant Component_List := Pattern.As_Component_List;
            Instance2 : constant Component_List := Instance.As_Component_List;
         begin
            return
              MP.Match (Pattern2.F_Components, Instance2.F_Components)
              and then MP.Match
                (Pattern2.F_Variant_Part, Instance2.F_Variant_Part);
         end;
      elsif Pattern.Kind = Ada_Single_Task_Decl then
         declare
            Pattern2 : constant Single_Task_Decl :=
              Pattern.As_Single_Task_Decl;
            Instance2 : constant Single_Task_Decl :=
              Instance.As_Single_Task_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Task_Type, Instance2.F_Task_Type);
         end;
      elsif Pattern.Kind = Ada_Exception_Decl then
         declare
            Pattern2  : constant Exception_Decl := Pattern.As_Exception_Decl;
            Instance2 : constant Exception_Decl := Instance.As_Exception_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match (Pattern2.F_Renames, Instance2.F_Renames);
         end;
      elsif Pattern.Kind = Ada_Abstract_Subp_Decl then
         declare
            Pattern2 : constant Abstract_Subp_Decl :=
              Pattern.As_Abstract_Subp_Decl;
            Instance2 : constant Abstract_Subp_Decl :=
              Instance.As_Abstract_Subp_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Overriding, Instance2.F_Overriding)
              and then MP.Match (Pattern2.F_Subp_Spec, Instance2.F_Subp_Spec);
         end;
      elsif Pattern.Kind = Ada_Ordinary_Fixed_Point_Def then
         declare
            Pattern2 : constant Ordinary_Fixed_Point_Def :=
              Pattern.As_Ordinary_Fixed_Point_Def;
            Instance2 : constant Ordinary_Fixed_Point_Def :=
              Instance.As_Ordinary_Fixed_Point_Def;
         begin
            return
              MP.Match (Pattern2.F_Delta, Instance2.F_Delta)
              and then MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Assign_Stmt then
         declare
            Pattern2  : constant Assign_Stmt := Pattern.As_Assign_Stmt;
            Instance2 : constant Assign_Stmt := Instance.As_Assign_Stmt;
         begin
            return
              MP.Match (Pattern2.F_Dest, Instance2.F_Dest)
              and then MP.Match (Pattern2.F_Expr, Instance2.F_Expr);
         end;
      elsif Pattern.Kind = Ada_Allocator then
         declare
            Pattern2  : constant Allocator := Pattern.As_Allocator;
            Instance2 : constant Allocator := Instance.As_Allocator;
         begin
            return
              MP.Match (Pattern2.F_Subpool, Instance2.F_Subpool)
              and then MP.Match
                (Pattern2.F_Type_Or_Expr, Instance2.F_Type_Or_Expr);
         end;
      elsif Pattern.Kind = Ada_Range_Constraint then
         declare
            Pattern2 : constant Range_Constraint :=
              Pattern.As_Range_Constraint;
            Instance2 : constant Range_Constraint :=
              Instance.As_Range_Constraint;
         begin
            return MP.Match (Pattern2.F_Range, Instance2.F_Range);
         end;
      elsif Pattern.Kind = Ada_Raise_Expr then
         declare
            Pattern2  : constant Raise_Expr := Pattern.As_Raise_Expr;
            Instance2 : constant Raise_Expr := Instance.As_Raise_Expr;
         begin
            return
              MP.Match (Pattern2.F_Exception_Name, Instance2.F_Exception_Name)
              and then MP.Match
                (Pattern2.F_Error_Message, Instance2.F_Error_Message);
         end;
      elsif Pattern.Kind = Ada_Use_Package_Clause then
         declare
            Pattern2 : constant Use_Package_Clause :=
              Pattern.As_Use_Package_Clause;
            Instance2 : constant Use_Package_Clause :=
              Instance.As_Use_Package_Clause;
         begin
            return MP.Match (Pattern2.F_Packages, Instance2.F_Packages);
         end;
      elsif Pattern.Kind = Ada_End_Name then
         declare
            Pattern2  : constant End_Name := Pattern.As_End_Name;
            Instance2 : constant End_Name := Instance.As_End_Name;
         begin
            return MP.Match (Pattern2.F_Name, Instance2.F_Name);
         end;
      elsif Pattern.Kind = Ada_Extended_Return_Stmt_Object_Decl then
         declare
            Pattern2 : constant Extended_Return_Stmt_Object_Decl :=
              Pattern.As_Extended_Return_Stmt_Object_Decl;
            Instance2 : constant Extended_Return_Stmt_Object_Decl :=
              Instance.As_Extended_Return_Stmt_Object_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Ids, Instance2.F_Ids)
              and then MP.Match
                (Pattern2.F_Has_Aliased, Instance2.F_Has_Aliased)
              and then MP.Match
                (Pattern2.F_Has_Constant, Instance2.F_Has_Constant)
              and then MP.Match (Pattern2.F_Mode, Instance2.F_Mode)
              and then MP.Match (Pattern2.F_Type_Expr, Instance2.F_Type_Expr)
              and then MP.Match
                (Pattern2.F_Default_Expr, Instance2.F_Default_Expr)
              and then MP.Match
                (Pattern2.F_Renaming_Clause, Instance2.F_Renaming_Clause);
         end;
      elsif Pattern.Kind = Ada_Public_Part then
         declare
            Pattern2  : constant Public_Part := Pattern.As_Public_Part;
            Instance2 : constant Public_Part := Instance.As_Public_Part;
         begin
            return MP.Match (Pattern2.F_Decls, Instance2.F_Decls);
         end;
      elsif Pattern.Kind = Ada_For_Loop_Spec then
         declare
            Pattern2  : constant For_Loop_Spec := Pattern.As_For_Loop_Spec;
            Instance2 : constant For_Loop_Spec := Instance.As_For_Loop_Spec;
         begin
            return
              MP.Match (Pattern2.F_Var_Decl, Instance2.F_Var_Decl)
              and then MP.Match (Pattern2.F_Loop_Type, Instance2.F_Loop_Type)
              and then MP.Match
                (Pattern2.F_Has_Reverse, Instance2.F_Has_Reverse)
              and then MP.Match (Pattern2.F_Iter_Expr, Instance2.F_Iter_Expr);
         end;
      elsif Pattern.Kind = Ada_Incomplete_Tagged_Type_Decl then
         declare
            Pattern2 : constant Incomplete_Tagged_Type_Decl :=
              Pattern.As_Incomplete_Tagged_Type_Decl;
            Instance2 : constant Incomplete_Tagged_Type_Decl :=
              Instance.As_Incomplete_Tagged_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match
                (Pattern2.F_Has_Abstract, Instance2.F_Has_Abstract);
         end;
      elsif Pattern.Kind = Ada_Aspect_Spec then
         declare
            Pattern2  : constant Aspect_Spec := Pattern.As_Aspect_Spec;
            Instance2 : constant Aspect_Spec := Instance.As_Aspect_Spec;
         begin
            return
              MP.Match (Pattern2.F_Aspect_Assocs, Instance2.F_Aspect_Assocs);
         end;
      elsif Pattern.Kind = Ada_Synth_Anonymous_Type_Decl then
         declare
            Pattern2 : constant Synth_Anonymous_Type_Decl :=
              Pattern.As_Synth_Anonymous_Type_Decl;
            Instance2 : constant Synth_Anonymous_Type_Decl :=
              Instance.As_Synth_Anonymous_Type_Decl;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match (Pattern2.F_Name, Instance2.F_Name)
              and then MP.Match
                (Pattern2.F_Discriminants, Instance2.F_Discriminants)
              and then MP.Match (Pattern2.F_Type_Def, Instance2.F_Type_Def);
         end;
      elsif Pattern.Kind = Ada_Generic_Package_Internal then
         declare
            Pattern2 : constant Generic_Package_Internal :=
              Pattern.As_Generic_Package_Internal;
            Instance2 : constant Generic_Package_Internal :=
              Instance.As_Generic_Package_Internal;
         begin
            return
              MP.Match (Pattern2.F_Aspects, Instance2.F_Aspects)
              and then MP.Match
                (Pattern2.F_Package_Name, Instance2.F_Package_Name)
              and then MP.Match
                (Pattern2.F_Public_Part, Instance2.F_Public_Part)
              and then MP.Match
                (Pattern2.F_Private_Part, Instance2.F_Private_Part)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;
      elsif Pattern.Kind = Ada_Case_Expr then
         declare
            Pattern2  : constant Case_Expr := Pattern.As_Case_Expr;
            Instance2 : constant Case_Expr := Instance.As_Case_Expr;
         begin
            return
              MP.Match (Pattern2.F_Expr, Instance2.F_Expr)
              and then MP.Match (Pattern2.F_Cases, Instance2.F_Cases);
         end;
      elsif Pattern.Kind = Ada_Library_Item then
         declare
            Pattern2  : constant Library_Item := Pattern.As_Library_Item;
            Instance2 : constant Library_Item := Instance.As_Library_Item;
         begin
            return
              MP.Match (Pattern2.F_Has_Private, Instance2.F_Has_Private)
              and then MP.Match (Pattern2.F_Item, Instance2.F_Item);
         end;
      elsif Pattern.Kind = Ada_Contract_Case_Assoc then
         declare
            Pattern2 : constant Contract_Case_Assoc :=
              Pattern.As_Contract_Case_Assoc;
            Instance2 : constant Contract_Case_Assoc :=
              Instance.As_Contract_Case_Assoc;
         begin
            return
              MP.Match (Pattern2.F_Guard, Instance2.F_Guard)
              and then MP.Match
                (Pattern2.F_Consequence, Instance2.F_Consequence);
         end;
      elsif Pattern.Kind = Ada_Discriminant_Constraint then
         declare
            Pattern2 : constant Discriminant_Constraint :=
              Pattern.As_Discriminant_Constraint;
            Instance2 : constant Discriminant_Constraint :=
              Instance.As_Discriminant_Constraint;
         begin
            return MP.Match (Pattern2.F_Constraints, Instance2.F_Constraints);
         end;
      elsif Pattern.Kind = Ada_Begin_Block then
         declare
            Pattern2  : constant Begin_Block := Pattern.As_Begin_Block;
            Instance2 : constant Begin_Block := Instance.As_Begin_Block;
         begin
            return
              MP.Match (Pattern2.F_Stmts, Instance2.F_Stmts)
              and then MP.Match (Pattern2.F_End_Name, Instance2.F_End_Name);
         end;

         -- End of generated fragment --------

      elsif Pattern.Kind in Ada_Ada_List then
         return
           MP.Match
             (Pattern.Children, Instance.Children, Instance.Children'First,
              True, False);
      elsif Pattern.Kind in Ada_String_Literal | Ada_Char_Literal then
         return Are_Equal_As_Raw_Signature (Pattern, Instance);
      elsif Pattern.Kind = Ada_Int_Literal then
         declare
            Pattern2  : constant Int_Literal := Pattern.As_Int_Literal;
            Instance2 : constant Int_Literal := Instance.As_Int_Literal;
         begin
            return Pattern2.P_Denoted_Value = Instance2.P_Denoted_Value;
         end;
      elsif Pattern.Children_Count = 0 and then Instance.Children_Count = 0
      then
         return
           Are_Equal_Case_Insensitive_As_Raw_Signature (Pattern, Instance);
      else
         Put_Line
           ("??? Rejuvenation.Match_Pattern: Match_Specific, " &
            "unknown Node type: " & Pattern.Image);
         return False;
      end if;
   end Match_Specific;

   procedure Dump_Partial_Match (MP : Match_Pattern) is
      use Mapping_Single_Map;
      use Mapping_Multiple_Map;
   begin
      Put_Line (Standard_Error, "Derived placeholder values:");
      for E in MP.Mapping_Single.Iterate loop
         Put_Line
           (Standard_Error,
            "* " & Key (E) & ": " & Raw_Signature (Element (E)));
      end loop;
      for E in MP.Mapping_Multiple.Iterate loop
         Put_Line
           (Standard_Error,
            "* " & Key (E) & ": [" & Element (E).Length'Image & "]");
         for N of Element (E) loop
            Put_Line (Standard_Error, "  - " & Raw_Signature (N));
         end loop;
      end loop;
   end Dump_Partial_Match;

   -- Private: Internal data structures --------

   function Equivalent_Key (Left, Right : String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;

   function Equivalent_Element (Left, Right : Ada_Node) return Boolean is
   begin
      Put_Line
        ("Check on Equivalent_Element: " & Left.Image & " & " & Right.Image);
      return False;
   end Equivalent_Element;

   function Equivalent_Element (Left, Right : Node_List.Vector) return Boolean
   is
   begin
      Put_Line
        ("Check on Equivalent_Element: " & Left.Length'Image & " & " &
         Right.Length'Image);
      return False;
   end Equivalent_Element;

end Rejuvenation.Match_Patterns;
