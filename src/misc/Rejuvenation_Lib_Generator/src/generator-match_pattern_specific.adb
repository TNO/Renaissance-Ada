with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Text_IO;                  use Ada.Text_IO;
with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Common;            use Libadalang.Common;
with Rejuvenation.Finder;
with Rejuvenation.Utils;

package body Generator.Match_Pattern_Specific is

   use Rejuvenation.Node_List;

   function Equivalent_Key (Left, Right : String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;

   function Equivalent_Value (Left, Right : List_String.Vector) return Boolean is
      use List_String;
   begin
      return Left = Right;
   end Equivalent_Value;

   Type_Decls : constant Rejuvenation.Node_List.Vector := Rejuvenation.Finder.Find (Unit_LAL_Ads.Root, Ada_Type_Decl);
   Subp_Decls : constant Rejuvenation.Node_List.Vector := Rejuvenation.Finder.Find (Unit_LAL_Ads.Root, Ada_Subp_Spec);

   Type_Decls2 : constant Rejuvenation.Node_List.Vector := Rejuvenation.Finder.Find (Unit_LALCO_Ads.Root, Ada_Type_Decl);

   Kinds : List_String.Vector;
   Type_To_Direct_Children, Type_To_All_Fields : Mapping_Single_Map.Map;

   procedure Append(Map : in out Mapping_Single_Map.Map; Key : String ; Value : String) is
   begin
      if not Map.Contains(Key) then
         declare
            List : List_String.Vector;
         begin
            Map.Include(Key, List);
         end;
      end if;
      declare
         List : List_String.Vector := Map.Element(Key);
         UValue : constant Unbounded_String := To_Unbounded_String(Value);
      begin
         if not List.Contains(UValue) then
            List.Append(UValue);
            Map.Include(Key, List);
         end if;
      end;
   end Append;

   procedure Process_Node_Kinds is
   begin
      for Type_Decl of Type_Decls2 loop
         declare
            Name : constant Libadalang.Analysis.Name := Type_Decl.As_Type_Decl.F_Name.F_Name;
            Str_Name : constant String := Rejuvenation.Utils.Raw_Signature(Name);
            Def : constant Type_Def := Type_Decl.As_Type_Decl.F_Type_Def;
         begin
            if Str_Name = "Ada_Node_Kind_Type" then
               for E of Def.As_Enum_Type_Def.F_Enum_Literals loop
                  Kinds.Append(To_Unbounded_String(Rejuvenation.Utils.Raw_Signature(E)));
               end loop;
            end if;
         end;

      end loop;
   end Process_Node_Kinds;

   procedure Process_Type_Decl is
   begin
      for Type_Decl of Type_Decls loop
         declare
            Name : constant Libadalang.Analysis.Name := Type_Decl.As_Type_Decl.F_Name.F_Name;
            Str_Name : constant String := Rejuvenation.Utils.Raw_Signature(Name);
            TypeDef : constant Type_Def := Type_Decl.As_Type_Decl.F_Type_Def;
         begin
            if TypeDef.Kind = Ada_Derived_Type_Def then
               if not TypeDef.As_Derived_Type_Def.F_Record_Extension.Is_Null then
                  declare
                     Sub_Type : constant Subtype_Indication := TypeDef.As_Derived_Type_Def.F_Subtype_Indication;
                     Sub_Type_Name : constant Libadalang.Analysis.Name := Sub_Type.F_Name;
                     Str_Sub_Type_Name : constant String := Rejuvenation.Utils.Raw_Signature(Sub_Type_Name);
                  begin
                     --Put_Line(Str_Name & " -> " & Str_Sub_Type_Name);
                     Append(Type_To_Direct_Children, Str_Sub_Type_Name, Str_Name);
                  end;
               end if;
            end if;
         end;
      end loop;
   end Process_Type_Decl;

   procedure Process_Subp is
      procedure Add_Recursive(Name : String; Type_Name : String) is
      begin
         if Type_To_Direct_Children.Contains(Type_Name) then
            for N of Type_To_Direct_Children(Type_Name) loop
               --Put_Line("  -> " & To_String(N));
               Append(Type_To_All_Fields, To_String(N), Name);
               Add_Recursive(Name, To_String(N));
            end loop;
         end if;
      end Add_Recursive;
   begin
      for Subp_Decl of Subp_Decls loop
         declare
            Subp_Name : constant Defining_Name := Subp_Decl.As_Subp_Spec.F_Subp_Name;
            Name : constant String := Rejuvenation.Utils.Raw_Signature(Subp_Name);
         begin
            if Index(Name, "F_") = 1 then
               declare
                  First_Param : constant Param_Spec := Subp_Decl.As_Subp_Spec.F_Subp_Params.F_Params.Child(1).As_Param_Spec;
                  First_Param_Type : constant Subtype_Indication := First_Param.F_Type_Expr.As_Subtype_Indication;
                  First_Param_Type_Name : constant Libadalang.Analysis.Name := First_Param_Type.F_Name.As_Attribute_Ref.F_Prefix;
                  Str_First_Param_Type_Name : constant String := Rejuvenation.Utils.Raw_Signature(First_Param_Type_Name);
               begin
                  --Put_Line(Name & " / " & Str_First_Param_Type_Name);
                  Append(Type_To_All_Fields, Str_First_Param_Type_Name, Name);
                  Add_Recursive(Name, Str_First_Param_Type_Name);
               end;
            end if;
         end;
      end loop;
   end Process_Subp;

   procedure Generate_Match_Specific is
   begin
      for E in Type_To_All_Fields.Iterate loop
         if Kinds.Contains(To_Unbounded_String("Ada_" & Mapping_Single_Map.Key(E))) then
            Put_Line("elsif Pattern.Kind = Ada_" & Mapping_Single_Map.Key(E) & " then");
            Put_Line("  declare");
            Put_Line("    Pattern2 : constant " & Mapping_Single_Map.Key(E) & " := Pattern.As_" & Mapping_Single_Map.Key(E) & ";");
            Put_Line("    Instance2 : constant " & Mapping_Single_Map.Key(E) & " := Instance.As_" & Mapping_Single_Map.Key(E) & ";");
            Put_Line("  begin");
            for N of Mapping_Single_Map.Element(E) loop
               declare
                  Prefix : Unbounded_String;
                  Postfix : Unbounded_String;
               begin
                  if N = Mapping_Single_Map.Element(E).First_Element then
                     Prefix := To_Unbounded_String("    return ");
                  else
                     Prefix := To_Unbounded_String("           ");
                  end if;
                  if N = Mapping_Single_Map.Element(E).Last_Element then
                     Postfix := To_Unbounded_String(";");
                  else
                     Postfix := To_Unbounded_String(" and then");
                  end if;
                  Put_Line(To_String(Prefix) & "MP.Match (Pattern2." & To_String(N) & ", Instance2." & To_String(N) & ")" & To_String(Postfix));
               end;
            end loop;
            Put_Line("  end;");
         end if;
      end loop;
   end;

   procedure Main is
   begin
      Process_Node_Kinds;
      Process_Type_Decl;
      Process_Subp;
      Generate_Match_Specific;
   end Main;

end Generator.Match_Pattern_Specific;
