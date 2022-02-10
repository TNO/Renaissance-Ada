with Rejuvenation.String_Utils; use Rejuvenation.String_Utils;
with Rejuvenation.Utils;        use Rejuvenation.Utils;
with Libadalang.Analysis;       use Libadalang.Analysis;

package body Predefined_Rewriters is

   Standard_Unit_Filename : constant String := "__standard";
   --  libadalang uses the standard unit for the standard type

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
      return
        Raw_Signature (T.F_Name) = Standard_Type_Name
        and then not T.P_Is_Array_Type
      --  not an array  (TODO: really needed?)

        and then Ends_With (T.Unit.Get_Filename, Standard_Unit_Filename);
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

end Predefined_Rewriters;
