with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

package body Rejuvenation.Factory is

   -- Project methods --------

   function Create_Context return Project_Context is
     (Context => Create_Context, others => <>);

   function Open_Project (Project_Path : String) return Project_Context is
      Result : Project_Context;
   begin
      Initialize (Result.Environment);
      Result.Project_Tree.Load (Create (+Project_Path), Result.Environment);
      Result.Provider :=
        Create_Project_Unit_Provider
          (Result.Project_Tree, Result.Project_Tree.Root_Project,
           Result.Environment);
      Result.Context := Create_Context (Unit_Provider => Result.Provider);
      return Result;
   end Open_Project;

   -- Core methods --------

   function Open_File
     (Filename : String; Context : Project_Context := Create_Context)
      return Analysis_Unit
   is
      Unit : constant Analysis_Unit :=
        Context.Context.Get_From_File (Filename);
      Empty_Or_Non_Existing_File_Exception : exception;
   begin
      if Unit.Root.Is_Null then
         raise Empty_Or_Non_Existing_File_Exception with Filename;
      else
         return Unit;
      end if;
   end Open_File;

   function Open_Files_From_Project
     (Context : Project_Context; Recursive : Boolean := True)
      return Analysis_Unit_Vectors.Vector
   is
      Files : File_Array renames
        Context.Project_Tree.Root_Project.Source_Files
          (Recursive => Recursive).all;
      Results : Analysis_Unit_Vectors.Vector;
   begin
      for File of Files loop
         declare
            Filename : constant String := +File.Full_Name;
         begin
            if Is_Ada_File_Built_By_Project (Filename, Context) then
               Results.Append (Open_File (Filename, Context));
            end if;
         end;
      end loop;
      return Results;
   end Open_Files_From_Project;

   function Is_Ada_File_Built_By_Project
     (Filename  : String; Context : Project_Context;
      Recursive : Boolean := True) return Boolean
   is
      Info : constant File_Info :=
        Context.Project_Tree.Info (Create (+Filename));
   begin
      if Recursive then
         return
           Info.Language = "ada"
           and then Info.Project /= No_Project
           and then not Info.Project.Externally_Built;
      else
         return
           Info.Language = "ada"
           and then Info.Project = Context.Project_Tree.Root_Project;
      end if;
   end Is_Ada_File_Built_By_Project;

   function Is_Project_Main_Program
     (Node : Ada_Node'Class; Context : Project_Context) return Boolean
   is
      Info : constant File_Info :=
        Context.Project_Tree.Info (Create (+Node.Unit.Get_Filename));
   begin
      return
        Info.Project.Is_Main_File (+Node.Unit.Get_Filename)
        and then Node.Kind in Ada_Basic_Decl
        and then Node.As_Basic_Decl.P_Is_Subprogram
        and then Node.As_Basic_Decl.P_Is_Compilation_Unit_Root;
   end Is_Project_Main_Program;

end Rejuvenation.Factory;
