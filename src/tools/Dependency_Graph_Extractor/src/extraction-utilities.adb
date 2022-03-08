with Ada.Characters.Handling;
with GNAT.OS_Lib;
with GNATCOLL.VFS_Utils;
with Libadalang.Project_Provider;

package body Extraction.Utilities is

   package LALPP renames Libadalang.Project_Provider;

   use type GPR.Project_Type;
   use type GPR.Unit_Parts;
   use type LALCO.Ada_Node_Kind_Type;
   use type VFS.Filesystem_String_Access;
   use type VFS.Virtual_File;

   function Open_Project (Project : String) return Project_Context is
   begin
      return Context : Project_Context do
         --  Note that the Unit_Provider becomes the owner of the Environment
         --  and Tree. Hence, we do not have to unload and free
         --  these ourselves. The Unit_Provider itself is reference counted
         --  and the Analysis_Context is a controlled type.
         --  Hence, we do not have to free these either.
         GPR.Initialize (Context.Project_Environment);
         Context.Project_Tree := new GPR.Project_Tree;
         Context.Project_Tree.Load
           (VFS.Create (+Project), Context.Project_Environment);
         Context.Unit_Provider :=
           LALPP.Create_Project_Unit_Provider
             (Context.Project_Tree, Context.Project_Tree.Root_Project,
              Context.Project_Environment);
         Context.Analysis_Context :=
           LAL.Create_Context (Unit_Provider => Context.Unit_Provider);
      end return;
   end Open_Project;

   function Get_Projects
     (Context : Project_Context; Recurse_Projects : Boolean)
      return Project_Vectors.Vector
   is
      Iter : GPR.Project_Iterator :=
        Context.Project_Tree.Root_Project.Start (Recurse_Projects);
      Current : GPR.Project_Type;
      Result  : Project_Vectors.Vector;
   begin
      loop
         Current := GPR.Current (Iter);
         exit when Current = GPR.No_Project;

         if not Current.Externally_Built then
            Result.Append (Current);
         end if;

         GPR.Next (Iter);
      end loop;

      return Result;
   end Get_Projects;

   function Open_Analysis_Units
     (Context : Project_Context; Recurse_Projects : Boolean)
      return Analysis_Unit_Vectors.Vector
   is
      Files : VFS.File_Array_Access :=
        Context.Project_Tree.Root_Project.Source_Files
          (Recursive => Recurse_Projects);
      Result : Analysis_Unit_Vectors.Vector;
   begin
      for File of Files.all loop
         declare
            Filename : constant String := +File.Full_Name;
         begin
            if Is_Ada_File_In_Project_Context
                (Filename, Context, Recurse_Projects)
            then
               Result.Append
                 (Context.Analysis_Context.Get_From_File (Filename));
            end if;
         end;
      end loop;

      VFS.Unchecked_Free (Files);

      return Result;
   end Open_Analysis_Units;

   function Is_Buildable_File_In_Project_Context
     (Filename : String; Context : Project_Context) return Boolean
   is
      File_Info : constant GPR.File_Info :=
        Context.Project_Tree.Info (VFS.Create (+Filename));
   begin
      return File_Info.Unit_Part /= GPR.Unit_Spec;
   end Is_Buildable_File_In_Project_Context;

   function Is_Ada_File_In_Project_Context
     (Ada_Filename     : String; Context : Project_Context;
      Recurse_Projects : Boolean) return Boolean
   is
      File_Info : constant GPR.File_Info :=
        Context.Project_Tree.Info (VFS.Create (+Ada_Filename));
   begin
      if Recurse_Projects then
         return
           File_Info.Language = "ada"
           and then File_Info.Project /= GPR.No_Project
           and then not File_Info.Project.Externally_Built;
      else
         return
           File_Info.Language = "ada"
           and then File_Info.Project = Context.Project_Tree.Root_Project;
      end if;
   end Is_Ada_File_In_Project_Context;

   function Is_Project_Main_Program
     (Node : LAL.Ada_Node'Class; Context : Project_Context) return Boolean
   is
      File_Info : constant GPR.File_Info :=
        Context.Project_Tree.Info (VFS.Create (+Node.Unit.Get_Filename));
   begin
      return
        File_Info.Project.Is_Main_File (+Node.Unit.Get_Filename)
        and then Node.Kind in LALCO.Ada_Basic_Decl
        and then Node.As_Basic_Decl.P_Is_Subprogram
        and then Node.As_Basic_Decl.P_Is_Compilation_Unit_Root;
   end Is_Project_Main_Program;

   function Get_Unique_Filename
     (Filename        : String; Directory_Prefix : VFS.Virtual_File;
      Make_Lower_Case : Boolean) return String
   is
      File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Filename);
   begin
      return Get_Unique_Filename (File, Directory_Prefix, Make_Lower_Case);
   end Get_Unique_Filename;

   function Get_GNATPRO_Directory_Prefix return VFS.Virtual_File;
   function Get_GNATPRO_Directory_Prefix return VFS.Virtual_File is
      Gnatls_Path : VFS.Filesystem_String_Access :=
        GNATCOLL.VFS_Utils.Locate_Exec_On_Path (+"gnatls");
   begin
      if Gnatls_Path = null then
         return VFS.No_File;
      end if;

      return Gnatls_Directory : VFS.Virtual_File do
         Gnatls_Directory := VFS.Create (Gnatls_Path.all).Get_Parent;
         VFS.Free (Gnatls_Path);

         if +Gnatls_Directory.Base_Dir_Name = "bin" then
            Gnatls_Directory := Gnatls_Directory.Get_Parent;
         end if;
      end return;
   end Get_GNATPRO_Directory_Prefix;

   GNATPRO_Directory_Prefix : constant VFS.Virtual_File :=
     Get_GNATPRO_Directory_Prefix;
   GNATPRO_Substitute_Prefix : constant String := "%GNATPRO_PATH%";

   function Get_Unique_Filename
     (File            : VFS.Virtual_File; Directory_Prefix : VFS.Virtual_File;
      Make_Lower_Case : Boolean) return String
   is

      function Get_Name return String;
      function Get_Name return String is
      begin
         if File = Standard_Unit_File then
            return Standard_Unit_Filename;
         elsif GNATPRO_Directory_Prefix /= VFS.No_File
           and then GNATPRO_Directory_Prefix.Is_Parent (File)
         then
            return
              GNATPRO_Substitute_Prefix & GNAT.OS_Lib.Directory_Separator &
              (+File.Relative_Path (GNATPRO_Directory_Prefix));
         elsif Directory_Prefix /= VFS.No_File
           and then Directory_Prefix.Is_Parent (File)
         then
            return +File.Relative_Path (Directory_Prefix);
         else
            return +File.Full_Name;
         end if;
      end Get_Name;

   begin
      if Make_Lower_Case then
         return Ada.Characters.Handling.To_Lower (Get_Name);
      else
         return Get_Name;
      end if;
   end Get_Unique_Filename;

   function Is_Subp_Body_Unit_With_Spec
     (Referenced_Decl : LAL.Basic_Decl) return Boolean;
   function Is_Subp_Body_Unit_With_Spec
     (Referenced_Decl : LAL.Basic_Decl) return Boolean
   is
   begin
      return
        Referenced_Decl.Parent.Kind = LALCO.Ada_Library_Item
        and then Referenced_Decl.Kind = LALCO.Ada_Subp_Body
        and then not Referenced_Decl.P_Previous_Part_For_Decl.Is_Null;
   end Is_Subp_Body_Unit_With_Spec;

   function Get_Referenced_Decl (Name : LAL.Name'Class) return LAL.Basic_Decl
   is
      Referenced_Decl : constant LAL.Basic_Decl := Name.P_Referenced_Decl;
   begin
      if Referenced_Decl.Is_Null then
         return Referenced_Decl;
      end if;

      --  In the case of a subprogram library unit use the specification and
      --  not the body when (a) the specification exists and (b) Name does
      --  not occur in the same unit as the body.
      if Is_Subp_Body_Unit_With_Spec (Referenced_Decl)
        and then Name.Unit.Root /= Referenced_Decl.Unit.Root
      then
         return Referenced_Decl.P_Previous_Part_For_Decl;
      else
         return Referenced_Decl;
      end if;
   end Get_Referenced_Decl;

   function Get_Referenced_Defining_Name
     (Name : LAL.Name'Class) return LAL.Defining_Name
   is
      Referenced_Defining_Name : constant LAL.Defining_Name :=
        Name.P_Referenced_Defining_Name;
      Referenced_Decl : constant LAL.Basic_Decl := Name.P_Referenced_Decl;
   begin
      --  In the case of a subprogram library unit use the defining name of the
      --  specification and not the body when (a) the specification exists and
      --  (b) Name does not occur in the same unit as the body.
      if Is_Subp_Body_Unit_With_Spec (Referenced_Decl)
        and then Name.Unit.Root /= Referenced_Decl.Unit.Root
      then
         if Referenced_Decl.P_Previous_Part_For_Decl.P_Defining_Names'Length =
           1
         then
            return Referenced_Decl.P_Previous_Part_For_Decl.P_Defining_Name;
         else
            raise Internal_Extraction_Error
              with "Basic declaration with single defining name expected";
         end if;
      else
         return Referenced_Defining_Name;
      end if;
   end Get_Referenced_Defining_Name;

   function Get_Parent_Basic_Decl
     (Node : LAL.Ada_Node'Class) return LAL.Basic_Decl
   is
      Parent_Basic_Decl : LAL.Basic_Decl := Node.P_Parent_Basic_Decl;
   begin
      if Parent_Basic_Decl.Is_Null then
         return Parent_Basic_Decl;
      end if;

      --  Skip stubs in case of their implementation, the implementation has
      --  the stub as its parent, this might confuse the user, who likely
      --  expects the package body containing the stub.
      if Node.Parent.Kind = LALCO.Ada_Subunit
        and then Node.Parent.As_Subunit.F_Body = Node
        and then Parent_Basic_Decl.Kind in LALCO.Ada_Body_Stub
      then
         Parent_Basic_Decl := Parent_Basic_Decl.P_Parent_Basic_Decl;
      end if;

      --  Skip exception handlers, their name (if any) is the choice parameter,
      --  these might confuse the user, who likely expects the name of the
      --  subprogram of which the exception handler is part.
      while Parent_Basic_Decl.Kind = LALCO.Ada_Exception_Handler loop
         Parent_Basic_Decl := Parent_Basic_Decl.P_Parent_Basic_Decl;
      end loop;

      --  Skip single task type declarations, these are internal AST nodes
      --  that we do not want to expose to the user.
      if Parent_Basic_Decl.Kind = LALCO.Ada_Single_Task_Type_Decl then
         Parent_Basic_Decl := Parent_Basic_Decl.P_Parent_Basic_Decl;
      end if;

      return Parent_Basic_Decl;
   end Get_Parent_Basic_Decl;

   function Occurs_In_Local_Context
     (Basic_Decl : LAL.Basic_Decl) return Boolean;
   function Occurs_In_Local_Context
     (Basic_Decl : LAL.Basic_Decl) return Boolean
   is
      Parent : constant LAL.Basic_Decl := Get_Parent_Basic_Decl (Basic_Decl);
      Kind   : constant LALCO.Ada_Node_Kind_Type := Parent.Kind;
   begin
      return
        Parent.P_Is_Subprogram
        or else
        (Kind = LALCO.Ada_Generic_Subp_Decl
         and then not Basic_Decl
           .P_Is_Formal) -- For consistency across formal parameters
        or else Kind = LALCO.Ada_Task_Body or else Kind = LALCO.Ada_Entry_Body;
   end Occurs_In_Local_Context;

   function Is_Relevant_Basic_Decl (Node : LAL.Ada_Node'Class) return Boolean
   is
      Kind : constant LALCO.Ada_Node_Kind_Type := Node.Kind;
   begin
      return
        Kind in LALCO.Ada_Basic_Decl
      --  Is nameless and, hence, cannot be referenced.

        and then Kind /= LALCO.Ada_Anonymous_Type_Decl
      --  Using children.

        and then Kind /= LALCO.Ada_Generic_Formal_Subp_Decl
      --  Children: Abstract_Formal_Subp_Decl and Concrete_Formal_Subp_Decl

        and then Kind /= LALCO.Ada_Generic_Formal_Type_Decl
      --  Child: Type_Decl

        and then Kind /= LALCO.Ada_Generic_Formal_Package
      --  Child: Generic_Package_Instantiation

        and then Kind /= LALCO.Ada_Generic_Formal_Obj_Decl
      --  Child: Object_Decl
      --  Using parent.

        and then Kind /= LALCO.Ada_Generic_Subp_Internal
      --  Parent: Generic_Subp_Decl

        and then Kind /= LALCO.Ada_Generic_Package_Internal
      --  Parent: Generic_Package_Decl

        and then Kind /= LALCO.Ada_Single_Task_Type_Decl
      --  Parent: Single_Task_Decl

        and then Kind /= LALCO.Ada_Exception_Handler
      --  Using parent to avoid confusion
      --  Is local and, hence, non-interesting in a global dependency view.

        and then Kind /= LALCO.Ada_Param_Spec
        and then Kind /= LALCO.Ada_For_Loop_Var_Decl
        and then Kind /= LALCO.Ada_Extended_Return_Stmt_Object_Decl
        and then Kind /= LALCO.Ada_Named_Stmt_Decl
        and then Kind /= LALCO.Ada_Label_Decl
        and then Kind /= LALCO.Ada_Entry_Index_Spec
      --  Is non-interesting when local.

        and then
        (Kind /= LALCO.Ada_Object_Decl
         or else not Occurs_In_Local_Context (Node.As_Basic_Decl))
        and then
        (Kind /= LALCO.Ada_Number_Decl
         or else not Occurs_In_Local_Context (Node.As_Basic_Decl));
   end Is_Relevant_Basic_Decl;

end Extraction.Utilities;
