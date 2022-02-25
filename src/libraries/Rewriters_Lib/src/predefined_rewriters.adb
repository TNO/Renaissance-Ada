with Ada.Assertions;            use Ada.Assertions;
with Rejuvenation.String_Utils; use Rejuvenation.String_Utils;
with Rejuvenation.Utils;        use Rejuvenation.Utils;

package body Predefined_Rewriters is

   --  Localize Ada Nodes in defining files

   Standard_Unit_Filename : constant String := "__standard";
   --  libadalang uses the standard unit for the standard type

   function Is_In_Standard_Unit (Node : Ada_Node'Class) return Boolean;
   function Is_In_Standard_Unit (Node : Ada_Node'Class) return Boolean
   is
   begin
      return Ends_With (Node.Unit.Get_Filename, Standard_Unit_Filename);
   end Is_In_Standard_Unit;

   function Is_In_AStrUnb (Node : Ada_Node'Class) return Boolean;
   function Is_In_AStrUnb (Node : Ada_Node'Class) return Boolean
   is
   begin
      return Ends_With (Node.Unit.Get_Filename, "\adainclude\a-strunb.ads");
   end Is_In_AStrUnb;

   --  Is Standard Ada Type checks

   function Is_Standard_Type_Expression
     (Match : Match_Pattern; Placeholder_Name, Standard_Type_Name : String)
      return Boolean;
   function Is_Standard_Type_Expression
     (Match : Match_Pattern; Placeholder_Name, Standard_Type_Name : String)
      return Boolean
   is
      T : constant Base_Type_Decl :=
        Get_Expression_Type (Match, Placeholder_Name);
   begin
      Assert (Check => not T.Is_Null,
              Message => "Is_Standard_Type_Expression - "
                        & "Unexpectedly Base Type Decl is null");
      return
        Raw_Signature (T.F_Name) = Standard_Type_Name
        and then Is_In_Standard_Unit (T);
   end Is_Standard_Type_Expression;

   function Is_Boolean_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression (Match, Placeholder_Name, "Boolean"));

   function Is_Integer_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression (Match, Placeholder_Name, "Integer"));

   function Is_Float_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression (Match, Placeholder_Name, "Float"));

   function Is_String_Expression
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean is
     (Is_Standard_Type_Expression (Match, Placeholder_Name, "String"));

   function Is_Unbounded_String
     (Match : Match_Pattern; Placeholder_Name : String) return Boolean
   is
      T : constant Base_Type_Decl :=
        Get_Expression_Type (Match, Placeholder_Name);
   begin
      return Raw_Signature (T.F_Name) = "Unbounded_String"
        and then Is_In_AStrUnb (T);
   end Is_Unbounded_String;

   --  For each function call, the function with the given name
   --  and type of arguments is matched.
   --  However the found function depends on the particular import relations,
   --  so we need to check if the defining file is right
   --  even when the name and types are checked!

   function Is_Referenced_Decl_Defined_In_AStrUnb
     (N : Name)
      return Boolean
   is
      Decl : constant Basic_Decl := N.P_Referenced_Decl;
   begin
      Assert (Check => not Decl.Is_Null,
              Message => "Is_Referenced_Decl_Defined_In_AStrUnb - "
                        & "Unexpectedly Decl is null");
      return Is_In_AStrUnb (Decl);
   end Is_Referenced_Decl_Defined_In_AStrUnb;

end Predefined_Rewriters;
