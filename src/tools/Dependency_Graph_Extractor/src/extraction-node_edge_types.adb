with Ada.Characters.Handling;
with GNATCOLL.VFS_Utils;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

package body Extraction.Node_Edge_Types is

   use type LALCO.Ada_Node_Kind_Type;
   use type VFS.Filesystem_String;
   use type VFS.Virtual_File;

   Decl_Name_Prefix : constant String := "decl:";
   Dir_Name_Prefix  : constant String := "dir:";
   File_Name_Prefix : constant String := "file:";
   Proj_Name_Prefix : constant String := "proj:";

   Filenames_Are_Case_Insensitive : constant Boolean :=
     not GNATCOLL.VFS_Utils.Local_Host_Is_Case_Sensitive;

   function Encode (Text : Langkit_Support.Text.Text_Type) return String is
     (Langkit_Support.Text.To_UTF8 (Text));

   function Encode
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return String is
     (Langkit_Support.Slocs.Image (Sloc_Range));

   function Encode (Value : Boolean) return String is
     (if Value then "true" else "false");

   function Get_Node_Name
     (File : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File)
      return String
   is
      Filename : constant String :=
        Utilities.Get_Unique_Filename
          (File, Directory_Prefix,
           Make_Lower_Case => Filenames_Are_Case_Insensitive);
   begin
      if File.Is_Directory then
         return Dir_Name_Prefix & Filename;
      else
         return File_Name_Prefix & Filename;
      end if;
   end Get_Node_Name;

   function Get_Node_Name
     (Project : GPR.Project_Type; Directory_Prefix : VFS.Virtual_File)
      return String
   is
      Filename : constant String :=
        Utilities.Get_Unique_Filename
          (Project.Project_Path, Directory_Prefix,
           Make_Lower_Case => Filenames_Are_Case_Insensitive);
      Name : constant String := Project.Name;
   begin
      return Proj_Name_Prefix & Filename & ":" & Name;
   end Get_Node_Name;

   function Get_Node_Name
     (Analysis_Unit : LAL.Analysis_Unit; Directory_Prefix : VFS.Virtual_File)
      return String
   is
      Filename : constant String :=
        Utilities.Get_Unique_Filename
          (Analysis_Unit.Get_Filename, Directory_Prefix,
           Make_Lower_Case => Filenames_Are_Case_Insensitive);
   begin
      return File_Name_Prefix & Filename;
   end Get_Node_Name;

   function Get_Node_Name
     (Defining_Name    : LAL.Defining_Name; Basic_Decl : LAL.Basic_Decl'Class;
      Directory_Prefix : VFS.Virtual_File) return String
   is
      Filename : constant String :=
        Utilities.Get_Unique_Filename
          (Defining_Name.Unit.Get_Filename, Directory_Prefix,
           Make_Lower_Case => Filenames_Are_Case_Insensitive);
      Name : constant String := Encode (Defining_Name.P_Relative_Name.Text);
      Sloc_Range : constant String := Encode (Basic_Decl.Sloc_Range);
   begin
      return Decl_Name_Prefix & Filename & ":" & Name & "[" & Sloc_Range & "]";
   end Get_Node_Name;

   function Node_Attributes return GW.Attribute_Definition_Sets.Map is
   begin
      return Attributes : GW.Attribute_Definition_Sets.Map do
         Attributes.Insert
           (Node_Attribute_Fully_Qualified_Name, GW.GraphML_String);
         Attributes.Insert
           (Node_Attribute_Is_Formal_Parameter, GW.GraphML_Boolean);
         Attributes.Insert
           (Node_Attribute_Is_Main_Program, GW.GraphML_Boolean);
         Attributes.Insert (Node_Attribute_Relative_Name, GW.GraphML_String);
         Attributes.Insert (Node_Attribute_Source_Location, GW.GraphML_String);
      end return;
   end Node_Attributes;

   function Get_Node_Attributes
     (File : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File)
      return GW.Attribute_Value_Sets.Map
   is
      --  Note that in the case of the fully quantified and relative names, we
      --  keep the original casing to keep the names as readable as possible.
      --  This differs from the behavior in the case of node names, where we
      --  normalize the casing depending on the case-sensitivity of file system
      --  to ensure that we do not get dupicate nodes on case-insensitive file
      --  systems due to file names having different casings in different parts
      --  of a codebase. As Extraction.Extract_Dependency_Graph first iterates
      --  over the file system to create nodes for all files and directories,
      --  it is ensured, via the implementation of Extraction.File_System,
      --  that the fully quantified and relative names that end up in our
      --  graph have casings that match the on-disk ones.
      Fully_Qualified_Name : constant SU.Unbounded_String :=
        +Utilities.Get_Unique_Filename
          (File, Directory_Prefix, Make_Lower_Case => False);
      Relative_Name : constant SU.Unbounded_String := +(+File.Base_Dir_Name);
   begin
      return Attributes : GW.Attribute_Value_Sets.Map do
         Attributes.Insert
           (Node_Attribute_Fully_Qualified_Name, Fully_Qualified_Name);
         Attributes.Insert (Node_Attribute_Relative_Name, Relative_Name);
      end return;
   end Get_Node_Attributes;

   function Get_Node_Attributes
     (Project : GPR.Project_Type) return GW.Attribute_Value_Sets.Map
   is
      Fully_Qualified_Name : constant SU.Unbounded_String := +Project.Name;
      Relative_Name        : constant SU.Unbounded_String := +Project.Name;
   begin
      return Attributes : GW.Attribute_Value_Sets.Map do
         Attributes.Insert
           (Node_Attribute_Fully_Qualified_Name, Fully_Qualified_Name);
         Attributes.Insert (Node_Attribute_Relative_Name, Relative_Name);
      end return;
   end Get_Node_Attributes;

   function Get_Node_Attributes
     (Defining_Name : LAL.Defining_Name; Basic_Decl : LAL.Basic_Decl;
      Context : Utilities.Project_Context) return GW.Attribute_Value_Sets.Map
   is
      Uninstantiated_Defining_Name : constant LAL.Defining_Name :=
        Defining_Name.P_Get_Uninstantiated_Node.As_Defining_Name;
      Uninstantiated_Basic_Decl : constant LAL.Basic_Decl :=
        Basic_Decl.P_Get_Uninstantiated_Node.As_Basic_Decl;
      Fully_Qualified_Name : constant String :=
        Encode (Uninstantiated_Defining_Name.P_Fully_Qualified_Name);
      Is_Formal_Parameter : constant String :=
        Encode (Uninstantiated_Basic_Decl.P_Is_Formal);
      Is_Main_Subprogram : constant String :=
        Encode
          (Utilities.Is_Project_Main_Program
             (Uninstantiated_Basic_Decl, Context));
      Relative_Name : constant String :=
        Encode (Uninstantiated_Defining_Name.P_Relative_Name.Text);
      Sloc_Range : constant String :=
        Encode (Uninstantiated_Basic_Decl.Sloc_Range);
   begin
      return Attributes : GW.Attribute_Value_Sets.Map do
         Attributes.Insert
           (Node_Attribute_Fully_Qualified_Name, +Fully_Qualified_Name);
         Attributes.Insert
           (Node_Attribute_Is_Formal_Parameter, +Is_Formal_Parameter);
         Attributes.Insert (Node_Attribute_Relative_Name, +Relative_Name);
         Attributes.Insert (Node_Attribute_Source_Location, +Sloc_Range);

         if Basic_Decl.P_Is_Subprogram then
            Attributes.Insert
              (Node_Attribute_Is_Main_Program, +Is_Main_Subprogram);
         end if;
      end return;
   end Get_Node_Attributes;

   function Get_File_Subtype (File : VFS.Virtual_File) return GW.Node_Subtype
   is
      Extension : constant String :=
        Ada.Characters.Handling.To_Lower (+File.File_Extension);
   begin
      if File = Utilities.Standard_Unit_File then
         return Node_Type_Ada_Specification_File;
      elsif Extension = ".adb" then
         return Node_Type_Ada_Body_File;
      elsif Extension = ".ads" then
         return Node_Type_Ada_Specification_File;
      elsif Extension = ".c" then
         return Node_Type_C_Source_File;
      elsif Extension = ".gpr" then
         return Node_Type_Gnat_Project_File;
      elsif Extension = ".h" then
         return Node_Type_C_Header_File;
      else
         return Node_Type_Unknown_File_Type;
      end if;
   end Get_File_Subtype;

   function Get_Decl_Subtype (Decl : LAL.Basic_Decl) return GW.Node_Subtype is
   begin
      case LALCO.Ada_Basic_Decl (Decl.Kind) is
         --  Components.
         when LALCO.Ada_Component_Decl =>
            return Node_Type_Ada_Component_Declaration;
            --  Discriminants.
         when LALCO.Ada_Discriminant_Spec =>
            return Node_Type_Ada_Discriminant_Declaration;
            --  Exceptions.
         when LALCO.Ada_Exception_Decl =>
            return Node_Type_Ada_Exception_Declaration;
            --  Numbers.
         when LALCO.Ada_Number_Decl =>
            return Node_Type_Ada_Number_Declaration;
            --  Objects.
         when LALCO.Ada_Object_Decl =>
            return Node_Type_Ada_Object_Declaration;
            --  Packages.
         when LALCO.Ada_Package_Decl | LALCO.Ada_Package_Body_Stub |
           LALCO.Ada_Package_Body | LALCO.Ada_Package_Renaming_Decl |
           LALCO.Ada_Generic_Package_Decl          |
           LALCO.Ada_Generic_Package_Instantiation |
           LALCO.Ada_Generic_Package_Renaming_Decl =>
            return Node_Type_Ada_Package_Declaration;
            --  Protected.
         when LALCO.Ada_Protected_Type_Decl | LALCO.Ada_Protected_Body_Stub |
           LALCO.Ada_Protected_Body | LALCO.Ada_Single_Protected_Decl =>
            return Node_Type_Ada_Protected_Declaration;
            --  Subprograms.
         when LALCO.Ada_Abstract_Subp_Decl | LALCO.Ada_Subp_Decl |
           LALCO.Ada_Expr_Function | LALCO.Ada_Null_Subp_Decl |
           LALCO.Ada_Subp_Body | LALCO.Ada_Subp_Renaming_Decl |
           LALCO.Ada_Subp_Body_Stub | LALCO.Ada_Generic_Subp_Decl |
           LALCO.Ada_Generic_Subp_Instantiation |
           LALCO.Ada_Generic_Subp_Renaming_Decl |
           LALCO.Ada_Abstract_Formal_Subp_Decl  |
           LALCO.Ada_Concrete_Formal_Subp_Decl  =>
            return Node_Type_Ada_Subprogram_Declaration;
         when LALCO.Ada_Entry_Decl | LALCO.Ada_Entry_Body =>
            return Node_Type_Ada_Entry_Declaration;
         when LALCO.Ada_Enum_Literal_Decl =>
            return Node_Type_Ada_Enum_Literal_Declaration;
            --  Tasks.
         when LALCO.Ada_Task_Type_Decl | LALCO.Ada_Task_Body_Stub |
           LALCO.Ada_Task_Body | LALCO.Ada_Single_Task_Decl =>
            return Node_Type_Ada_Task_Declaration;
            --  Types.
         when LALCO.Ada_Type_Decl | LALCO.Ada_Subtype_Decl |
           LALCO.Ada_Incomplete_Type_Decl        |
           LALCO.Ada_Incomplete_Tagged_Type_Decl =>
            return Node_Type_Ada_Type_Declaration;
         when others =>
            raise Internal_Extraction_Error
              with "Unhandled basic declaration: " & Decl.Kind'Image;
      end case;
   end Get_Decl_Subtype;

   function Edge_Attributes return GW.Attribute_Definition_Sets.Map is
   begin
      return Attributes : GW.Attribute_Definition_Sets.Map do
         Attributes.Insert
           (Edge_Attribute_Has_Access_Type, GW.GraphML_Boolean);
         Attributes.Insert (Edge_Attribute_Has_Array_Type, GW.GraphML_Boolean);
         Attributes.Insert (Edge_Attribute_Is_Dispatching, GW.GraphML_Boolean);
      end return;
   end Edge_Attributes;

   function Get_Edge_Attributes
     (Expr : LAL.Expr'Class) return GW.Attribute_Value_Sets.Map
   is
      Is_Dispatching : constant String := Encode (Expr.P_Is_Dispatching_Call);
   begin
      return Attributes : GW.Attribute_Value_Sets.Map do
         Attributes.Insert (Edge_Attribute_Is_Dispatching, +Is_Dispatching);
      end return;
   end Get_Edge_Attributes;

   function Get_Edge_Attributes
     (Type_Expr : LAL.Type_Expr'Class; Can_Have_Array_Type : Boolean)
      return GW.Attribute_Value_Sets.Map
   is

      Has_Access_Type : Boolean := False;
      Has_Array_Type  : Boolean := False;

      procedure Get_Attribute_Values (Type_Def : LAL.Type_Def);

      procedure Get_Attribute_Values (Type_Expr : LAL.Type_Expr'Class);
      procedure Get_Attribute_Values (Type_Expr : LAL.Type_Expr'Class) is
      begin
         if Type_Expr.Kind = LALCO.Ada_Anonymous_Type then
            Get_Attribute_Values
              (Type_Expr.As_Anonymous_Type.F_Type_Decl.F_Type_Def);
         end if;
      end Get_Attribute_Values;

      procedure Get_Attribute_Values (Type_Def : LAL.Type_Def) is
      begin
         if Type_Def.Kind = LALCO.Ada_Array_Type_Def then
            Has_Array_Type := True;
            Get_Attribute_Values
              (Type_Def.As_Array_Type_Def.F_Component_Type.F_Type_Expr);
         elsif Type_Def.Kind = LALCO.Ada_Type_Access_Def then
            Has_Access_Type := True;
            Get_Attribute_Values
              (Type_Def.As_Type_Access_Def.F_Subtype_Indication);
         end if;
      end Get_Attribute_Values;

   begin
      Get_Attribute_Values (Type_Expr);

      return Attributes : GW.Attribute_Value_Sets.Map do
         Attributes.Insert
           (Edge_Attribute_Has_Access_Type, +Encode (Has_Access_Type));

         if Can_Have_Array_Type then
            Attributes.Insert
              (Edge_Attribute_Has_Array_Type, +Encode (Has_Array_Type));
         elsif Has_Array_Type then
            raise Internal_Extraction_Error
              with "Unexpected Has_Array_Type in Get_Edge_Attributes";
         end if;
      end return;
   end Get_Edge_Attributes;

end Extraction.Node_Edge_Types;
