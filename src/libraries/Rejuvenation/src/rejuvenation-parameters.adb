with Ada.Assertions;     use Ada.Assertions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;        use Ada.Text_IO;
with Rejuvenation.Utils; use Rejuvenation.Utils;

package body Rejuvenation.Parameters is

   function To_Param_Default_Array
     (Children : Ada_Node_Array; Default_Expr : Expr)
      return Param_Actual_Array with
      Post => (To_Param_Default_Array'Result'First = Children'First)
      and then (To_Param_Default_Array'Result'Last = Children'Last);

   function To_Param_Default_Array
     (Children : Ada_Node_Array; Default_Expr : Expr) return Param_Actual_Array
   is
   begin
      return Result : Param_Actual_Array (Children'Range) do
         for Index in Children'Range loop
            declare
               Def_Name : constant Defining_Name :=
                 Children (Index).As_Defining_Name;
            begin
               Result (Index) := Create_Param_Actual (Def_Name, Default_Expr);
            end;
         end loop;
      end return;
   end To_Param_Default_Array;

   function To_Param_Default_Array
     (P : Param_Spec) return Param_Actual_Array with
      Pre => P.F_Ids.Children'Last >=
      P.F_Ids.Children'First,  -- Enforced by ada language
      Post => (To_Param_Default_Array'Result'First = P.F_Ids.Children'First)
      and then (To_Param_Default_Array'Result'Last = P.F_Ids.Children'Last);

   function To_Param_Default_Array (P : Param_Spec) return Param_Actual_Array
   is
   begin
      return To_Param_Default_Array (P.F_Ids.Children, P.F_Default_Expr);
   end To_Param_Default_Array;

   function To_Param_Default_Array
     (PL : Param_Spec_List) return Param_Actual_Array with
      Pre =>
      (for all P of PL => P.F_Ids.Children'Last >= P.F_Ids.Children'First);

   function To_Param_Default_Array
     (PL : Param_Spec_List) return Param_Actual_Array
   is
      Return_Length : Integer := 0;
   begin
      for P of PL loop
         Return_Length := Return_Length + P.F_Ids.Children'Length;
      end loop;

      return Result : Param_Actual_Array (1 .. Return_Length) do
         declare
            Low : Integer := 1;
         begin
            for P of PL loop
               declare
                  PAA : constant Param_Actual_Array :=
                    To_Param_Default_Array (P.As_Param_Spec);
                  High : constant Integer := Low + PAA'Length - 1;
               begin
                  Result (Low .. High) := PAA;
                  Low                  := High + 1;
               end;
            end loop;
         end;
      end return;
   end To_Param_Default_Array;

   function To_Param_Default_Array
     (Node : Subp_Spec) return Param_Actual_Array;

   function To_Param_Default_Array (Node : Subp_Spec) return Param_Actual_Array
   is
   begin
      if Node.F_Subp_Params.Is_Null then
         return Result : Param_Actual_Array (1 .. 0);
      else
         return To_Param_Default_Array (Node.F_Subp_Params.F_Params);
      end if;
   end To_Param_Default_Array;

   function Param_Expressions
     (Nm : Libadalang.Analysis.Name) return Param_Actual_Array
   is
   begin
      Assert
        (Check   => not Nm.Is_Null,
         Message => "Precondition violated: Name is Null");
      Assert
        (Check   => Nm.P_Is_Call,
         Message => "Precondition violated: Name is not a Call");
      declare
         BFPH : constant Base_Formal_Param_Holder := Nm.P_Called_Subp_Spec;
         SS   : constant Subp_Spec                := BFPH.As_Subp_Spec;
      begin
         Assert
           (Check   => not SS.Is_Null,
            Message => "SS is null => BFPH.Kind = " & BFPH.Kind'Image);
         declare
            All_Param_Actual_Array : Param_Actual_Array :=
              To_Param_Default_Array (SS);
         begin
            if All_Param_Actual_Array'Length > 0 then
               case Nm.Kind is
                  when Ada_Identifier =>
                     --  Nothing to handle (no arguments provided)
                     null;
                  when Ada_Op =>
                     case Nm.Parent.Kind is
                        when Ada_Bin_Op | Ada_Relation_Op =>
                           --  Handle the two provided arguments
                           declare
                              BO : constant Bin_Op := Nm.Parent.As_Bin_Op;
                           begin
                              Assert
                                (Check   => All_Param_Actual_Array'Length = 2,
                                 Message =>
                                   "Binary Operator has unexpected length " &
                                   "of " &
                                   All_Param_Actual_Array'Length'Image);
                              All_Param_Actual_Array
                                (All_Param_Actual_Array'First) :=
                                Create_Param_Actual
                                  (Param
                                     (All_Param_Actual_Array
                                        (All_Param_Actual_Array'First)),
                                   BO.F_Left);
                              All_Param_Actual_Array
                                (All_Param_Actual_Array'Last) :=
                                Create_Param_Actual
                                  (Param
                                     (All_Param_Actual_Array
                                        (All_Param_Actual_Array'Last)),
                                   BO.F_Right);
                           end;
                        when Ada_Un_Op =>
                           --  Handle the single provided argument
                           declare
                              UO : constant Un_Op := Nm.Parent.As_Un_Op;
                           begin
                              Assert
                                (Check   => All_Param_Actual_Array'Length = 1,
                                 Message =>
                                   "Unary Operator has unexpected length " &
                                   "of " &
                                   All_Param_Actual_Array'Length'Image);
                              All_Param_Actual_Array
                                (All_Param_Actual_Array'First) :=
                                Create_Param_Actual
                                  (Param
                                     (All_Param_Actual_Array
                                        (All_Param_Actual_Array'First)),
                                   UO.F_Expr);
                           end;
                        when others =>
                           Assert
                             (Check   => False,
                              Message =>
                                "Unexpected Parent of kind " &
                                Nm.Parent.Kind'Image);

                     end case;
                  when Ada_Call_Expr =>
                     --  Handle provided arguments
                     declare
                        CE : constant Call_Expr := Nm.As_Call_Expr;
                     begin
                        Assert
                          (Check   => CE.F_Suffix.Kind = Ada_Assoc_List,
                           Message =>
                             "Expected Assoc_List yet got " &
                             CE.F_Suffix.Kind'Image);
                        declare
                           AL : constant Assoc_List :=
                             CE.F_Suffix.As_Assoc_List;
                        begin
                           for Param_Expr of AL.P_Zip_With_Params loop
                              for I in All_Param_Actual_Array'Range
                              loop -- TODO: can this be done more efficiently?
                                 if Ada.Strings.Equal_Case_Insensitive
                                     (Raw_Signature (Param (Param_Expr)),
                                      Raw_Signature
                                        (Param (All_Param_Actual_Array (I))))
                                 then
                                    All_Param_Actual_Array (I) := Param_Expr;
                                 end if;
                              end loop;
                           end loop;
                        end;
                        --  Handle Prefix Notation
                        if Actual
                            (All_Param_Actual_Array
                               (All_Param_Actual_Array'First)) =
                          No_Expr
                        then
                           Assert
                             (Check   => CE.F_Name.Kind = Ada_Dotted_Name,
                              Message =>
                                "Call Expr - Only expecting no First Value " &
                                "in case of prefix notaton: " &
                                "Dotted Name, yet " & CE.F_Name.Kind'Image);
                           All_Param_Actual_Array
                             (All_Param_Actual_Array'First) :=
                             Create_Param_Actual
                               (Param
                                  (All_Param_Actual_Array
                                     (All_Param_Actual_Array'First)),
                                CE.F_Name.As_Dotted_Name.F_Prefix);
                        end if;
                     end;
                  when Ada_Dotted_Name =>
                     --  Handle Prefix Notation
                     if Actual
                         (All_Param_Actual_Array
                            (All_Param_Actual_Array'First)) =
                       No_Expr
                     then
                        All_Param_Actual_Array
                          (All_Param_Actual_Array'First) :=
                          Create_Param_Actual
                            (Param
                               (All_Param_Actual_Array
                                  (All_Param_Actual_Array'First)),
                             Nm.As_Dotted_Name.F_Prefix);
                     end if;
                  when others =>
                     Assert
                       (Check   => False,
                        Message =>
                          "Unexpectedly no First Value for " & Nm.Kind'Image);
               end case;
            end if;

            return All_Param_Actual_Array;
         end;
      end;
   end Param_Expressions;

   function To_Param_Default_Array
     (BD : Generic_Formal) return Param_Actual_Array;

   function To_Param_Default_Array
     (BD : Generic_Formal) return Param_Actual_Array
   is
   begin
      case BD.Kind is
         when Ada_Generic_Formal_Obj_Decl =>
            declare
               GFOD : constant Generic_Formal_Obj_Decl :=
                 BD.As_Generic_Formal_Obj_Decl;
               OD       : constant Object_Decl := GFOD.F_Decl.As_Object_Decl;
               Children : constant Ada_Node_Array := OD.F_Ids.Children;
            begin
               return To_Param_Default_Array (Children, OD.F_Default_Expr);
            end;
         when Ada_Generic_Formal_Subp_Decl =>
            declare
               GFSD : constant Generic_Formal_Subp_Decl :=
                 BD.As_Generic_Formal_Subp_Decl;
               FSD : constant Formal_Subp_Decl :=
                 GFSD.F_Decl.As_Formal_Subp_Decl;
            begin
               --  TODO: how to implement default value box (expression)?
               --  see Ada Reference Manual - 12.6 Formal Subprograms
               --  http://www.ada-auth.org/standards/12rm/html/RM-12-6.html
               return Result : Param_Actual_Array (1 .. 1) do
                  Result (1) :=
                    Create_Param_Actual
                      (FSD.F_Subp_Spec.F_Subp_Name, FSD.F_Default_Expr);
               end return;
            end;
         when Ada_Generic_Formal_Type_Decl =>
            declare
               GFTD : constant Generic_Formal_Type_Decl :=
                 BD.As_Generic_Formal_Type_Decl;
               TD : constant Type_Decl := GFTD.F_Decl.As_Type_Decl;
            begin
               return Result : Param_Actual_Array (1 .. 1) do
                  Result (1) :=
                    Create_Param_Actual
                      (TD.F_Name,
                       No_Expr); -- Types have no default- check with Jeroen
               end return;
            end;
         when Ada_Generic_Formal_Package =>
            declare
               GFP : constant Generic_Formal_Package :=
                 BD.As_Generic_Formal_Package;
            begin
               case GFP.F_Decl.Kind is
                  when Ada_Package_Decl =>
                     declare
                        PD : constant Package_Decl :=
                          GFP.F_Decl.As_Package_Decl;
                     begin
                        return Result : Param_Actual_Array (1 .. 1) do
                           Result (1) :=
                             Create_Param_Actual (PD.F_Package_Name,
                           --  Generic Formal Package has no default
                           No_Expr);

                        end return;
                     end;
                  when Ada_Generic_Package_Instantiation =>
                     declare
                        GPI : constant Generic_Package_Instantiation :=
                          GFP.F_Decl.As_Generic_Package_Instantiation;
                     begin
                        return Result : Param_Actual_Array (1 .. 1) do
                           Result (1) :=
                             Create_Param_Actual (GPI.F_Name,
                           --  Generic Package Instantiation has no default
                           No_Expr);
                        end return;
                     end;
                  when others =>
                     Put_Line
                       ("Unexpected Decl kind " & GFP.F_Decl.Kind'Image);
                     return Result : Param_Actual_Array (1 .. 0);
               end case;
            end;
         when others =>
            Put_Line ("Unexpected kind " & BD.Kind'Image);
            return Result : Param_Actual_Array (1 .. 0);
      end case;
   exception
      when others =>
         Put_Line
           ("Unexpected Exception (line 158) " & BD.Image & " in " &
            BD.Unit.Get_Filename);
         BD.Print;
         return Result : Param_Actual_Array (1 .. 0);
   end To_Param_Default_Array;

   function To_Param_Default_Array
     (ANL : Ada_Node_List) return Param_Actual_Array;

   function To_Param_Default_Array
     (ANL : Ada_Node_List) return Param_Actual_Array
   is
      Return_Length : Integer := 0;
   begin
      for AN of ANL loop
         --  only get parameters defined in Ada_Node_List
         --  (and e.g. ignore Ada_Use_Package_Clause)
         if AN.Kind in Ada_Generic_Formal then
            Return_Length :=
              Return_Length +
              To_Param_Default_Array (AN.As_Generic_Formal)'Length;
         end if;
      end loop;

      return Result : Param_Actual_Array (1 .. Return_Length) do
         declare
            Low : Integer := 1;
         begin
            for AN of ANL loop
               if AN.Kind in Ada_Generic_Formal then
                  declare
                     PAA : constant Param_Actual_Array :=
                       To_Param_Default_Array (AN.As_Generic_Formal);
                     High : constant Integer := Low + PAA'Length - 1;
                  begin
                     Result (Low .. High) := PAA;
                     Low                  := High + 1;
                  end;
               end if;
            end loop;
         end;
      end return;
   exception
      when others =>
         Put_Line
           ("Unexpected Exception (line 192) " & ANL.Image & " in " &
            ANL.Unit.Get_Filename);
         ANL.Print;
         return Result : Param_Actual_Array (1 .. 0);
   end To_Param_Default_Array;

   function To_Param_Default_Array
     (GPD : Generic_Package_Decl) return Param_Actual_Array;

   function To_Param_Default_Array
     (GPD : Generic_Package_Decl) return Param_Actual_Array
   is
   begin
      return To_Param_Default_Array (GPD.F_Formal_Part.F_Decls);
   end To_Param_Default_Array;

   function Param_Expressions
     (GPI : Generic_Package_Instantiation) return Param_Actual_Array
   is
      AL                     : constant Assoc_List := GPI.F_Params;
      GPN : constant Libadalang.Analysis.Name := GPI.F_Generic_Pkg_Name;
      RefDecl                : constant Basic_Decl := GPN.P_Referenced_Decl;
      GPD : constant Generic_Package_Decl := RefDecl.As_Generic_Package_Decl;
      All_Param_Actual_Array : Param_Actual_Array                :=
        To_Param_Default_Array (GPD);
   begin
      for Param_Expr of AL.P_Zip_With_Params loop
         for I in All_Param_Actual_Array'Range
         loop -- TODO: can this be done more efficiently?
            if Param (Param_Expr) = Param (All_Param_Actual_Array (I)) then
               All_Param_Actual_Array (I) := Param_Expr;
            end if;
         end loop;
      end loop;
      return All_Param_Actual_Array;
   end Param_Expressions;

   function Param_Expressions (Stat : Call_Stmt) return Param_Actual_Array is
      Call    : constant Libadalang.Analysis.Name := Stat.F_Call;
      Default : Param_Actual_Array (1 .. 0);
   begin
      if Call.Kind = Ada_Call_Expr then
         declare
            Called_Name : constant Libadalang.Analysis.Name :=
              Call.As_Call_Expr.F_Name;
         begin
            return Param_Expressions (Called_Name);
         end;
      end if;
      return Default;
   end Param_Expressions;

   function Get_Expression
     (The_Array : Param_Actual_Array; Name : String) return Expr'Class
   is
   begin
      for Param_Expr of The_Array loop
         if Name = Raw_Signature (Param (Param_Expr)) then
            return Actual (Param_Expr);
         end if;
      end loop;
      return No_Expr;
   end Get_Expression;

end Rejuvenation.Parameters;
