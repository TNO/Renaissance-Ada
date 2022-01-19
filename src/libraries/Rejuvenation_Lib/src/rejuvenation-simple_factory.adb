with Ada.Directories;             use Ada.Directories;
with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;

package body Rejuvenation.Simple_Factory is

   function Diagnostics_To_String (Unit : Analysis_Unit'Class) return String;
   --  Diagnostics to string
   --  ends with line feed

   function Diagnostics_To_String (Unit : Analysis_Unit'Class) return String is
      Message : Unbounded_String;
   begin
      for Diagnostic of Unit.Diagnostics loop
         Message :=
           Message & Unit.Format_GNU_Diagnostic (Diagnostic) & ASCII.LF;
      end loop;
      return To_String (Message);
   end Diagnostics_To_String;

   procedure Put_Diagnostics (File : File_Type; Unit : Analysis_Unit) is
   begin
      Put (File, Diagnostics_To_String (Unit));
   end Put_Diagnostics;

   procedure Put_Diagnostics (Unit : Analysis_Unit) is
   begin
      Put (Diagnostics_To_String (Unit));
   end Put_Diagnostics;

   function Analyze_Fragment
     (Fragment : String; Rule : Grammar_Rule := Default_Grammar_Rule)
      return Analysis_Unit
   is
      Context : constant Analysis_Context := Create_Context;
      Unit    : constant Analysis_Unit    :=
        Context.Get_From_Buffer ("Fragment", "", Fragment, Rule);
   begin
      if Unit.Has_Diagnostics then
         Put_Line ("On");
         Put_Line (Fragment);
         Put_Line ("Parse Exception");
         Put_Diagnostics (Unit);

         raise Parse_Exception
           with Diagnostics_To_String (Unit) & "On" & ASCII.LF & Fragment;
      else
         return Unit;
      end if;
   end Analyze_Fragment;

   function Analyze_File_In_Context
     (Filename : String; Context : Analysis_Context'Class)
      return Analysis_Unit;

   function Analyze_File_In_Context
     (Filename : String; Context : Analysis_Context'Class) return Analysis_Unit
   is
      Unit : constant Analysis_Unit := Context.Get_From_File (Filename);
   begin
      if Unit.Has_Diagnostics then
         raise Parse_Exception
           with Diagnostics_To_String (Unit) & "In" & ASCII.LF & Filename;
      else
         return Unit;
      end if;
   end Analyze_File_In_Context;

   function Analyze_File (Filename : String) return Analysis_Unit is
      Context : constant Analysis_Context := Create_Context;
   begin
      return Analyze_File_In_Context (Filename, Context);
   end Analyze_File;

   function Analyze_File_In_Project
     (Filename : String; Project_Filename : String) return Analysis_Unit
   is
      Project_File : constant Virtual_File := Create (+Project_Filename);
      Env          : Project_Environment_Access;
      Project      : constant Project_Tree_Access := new Project_Tree;
   begin
      Initialize (Env);
      Project.Load (Project_File, Env);
      declare
         Provider : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider (Project, Project.Root_Project, Env);
         Context : constant Analysis_Context :=
           Create_Context (Unit_Provider => Provider);
      begin
         return Analyze_File_In_Context (Filename, Context);
      end;
   end Analyze_File_In_Project;

   function Is_Ada_File
     (Tree : Project_Tree_Access; File : Virtual_File) return Boolean;

   function Is_Ada_File
     (Tree : Project_Tree_Access; File : Virtual_File) return Boolean
   is
      Info : constant File_Info :=
        Tree.Info (Create (+File.Display_Full_Name));
   begin
      return Info.Language = "ada";
   end Is_Ada_File;

   function Get_Ada_Source_Files_From_Project
     (Project_Filename : String; Recursive : Boolean := True)
      return Unbounded_Strings.Vector
   is
      Project_File : constant Virtual_File := Create (+Project_Filename);
      Env          : Project_Environment_Access;
      Project      : constant Project_Tree_Access := new Project_Tree;
      Results      : Unbounded_Strings.Vector;
   begin
      Initialize (Env);
      Project.Load (Project_File, Env);
      for File of Project.Root_Project.Source_Files
        (Recursive => Recursive, Include_Externally_Built => False).all
      loop
         if Is_Ada_File (Project, File) then
            Results.Append (To_Unbounded_String (+File.Full_Name));
         end if;
      end loop;
      return Results;
   end Get_Ada_Source_Files_From_Project;

   function Get_Ada_Source_Files_From_Directory
     (Directory_Name : String; Recursive : Boolean := True)
      return Unbounded_Strings.Vector
   is
      Results : Unbounded_Strings.Vector;

      procedure Append (Item : Directory_Entry_Type);

      procedure Append (Item : Directory_Entry_Type) is
      begin
         Results.Append (To_Unbounded_String (Full_Name (Item)));
      end Append;
   begin
      Walk_Files
        (Directory_Name, File_Pattern => "*.ad[bs]",
         Process_File => Append'Access, Recursive => Recursive);
      return Results;
   end Get_Ada_Source_Files_From_Directory;

   function Analyze_Project
     (Project_Filename : String; Recursive : Boolean := True)
      return Analysis_Units.Vector
   is
      Project_File : constant Virtual_File := Create (+Project_Filename);
      Env          : Project_Environment_Access;
      Project      : constant Project_Tree_Access := new Project_Tree;
   begin
      Initialize (Env);
      Project.Load (Project_File, Env);
      declare
         Provider : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider (Project, Project.Root_Project, Env);
         Context : constant Analysis_Context :=
           Create_Context (Unit_Provider => Provider);
         Results : Analysis_Units.Vector;
      begin
         for File of Project.Root_Project.Source_Files
           (Recursive => Recursive, Include_Externally_Built => False).all
         loop
            if Is_Ada_File (Project, File) then
               Results.Append
                 (Analyze_File_In_Context (+File.Full_Name, Context));
            end if;
         end loop;
         return Results;
      end;
   end Analyze_Project;

end Rejuvenation.Simple_Factory;
