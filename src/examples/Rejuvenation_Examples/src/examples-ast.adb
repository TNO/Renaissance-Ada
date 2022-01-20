with Ada.Text_IO;          use Ada.Text_IO;
with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Rejuvenation;         use Rejuvenation;
with Rejuvenation.Factory; use Rejuvenation.Factory;
with Rejuvenation.Finder;  use Rejuvenation.Finder;

package body Examples.Ast is

   procedure Demo_Syntactic_F_Fields (Unit : Analysis_Unit);
   procedure Demo_Semantic_P_Properties (Unit : Analysis_Unit);

   procedure Demo (Project_Name : String; File_Name : String) is
      Context : constant Project_Context := Open_Project (Project_Name);
      Unit    : constant Analysis_Unit   := Open_File (File_Name, Context);
   begin
      Put_Line ("=== Examples of AST accessors =======");
      New_Line;

      Put_Line ("--- Example of syntactic F fields -------");
      New_Line;
      Demo_Syntactic_F_Fields (Unit);
      New_Line;

      Put_Line ("--- Example of semantic P properties -------");
      New_Line;
      Demo_Semantic_P_Properties (Unit);
      New_Line;
   end Demo;

   procedure Demo_Syntactic_F_Fields (Unit : Analysis_Unit) is
      --  Hint: <CTRL>-<SPACE> helps to find the right F_ fields
      CU    : constant Compilation_Unit := Unit.Root.As_Compilation_Unit;
      LI    : constant Library_Item     := CU.F_Body.As_Library_Item;
      Subp  : constant Subp_Body        := LI.F_Item.As_Subp_Body;
      DP    : constant Declarative_Part := Subp.F_Decls;
      Decls : constant Ada_Node_List    := DP.F_Decls;
   begin
      Put_Line ("Declarations:");
      for Decl of Decls loop
         New_Line;
         Decl.Print; -- Show the node's internal structure
      end loop;
   end Demo_Syntactic_F_Fields;

   procedure Demo_Semantic_P_Properties (Unit : Analysis_Unit) is
   --  Hint: <CTRL>-<SPACE> helps to find the
   --  right F_ and P_ fields/properties
   begin
      for Node of Find (Unit.Root, Ada_Call_Expr) loop
         declare
            Call      : constant Call_Expr                := Node.As_Call_Expr;
            Call_Name : constant Libadalang.Analysis.Name := Call.F_Name;
         begin
            Put_Line
              ("Call: " & Call.Image & " from " & Call.Unit.Get_Filename);
            declare
               Called_Decl : constant Basic_Decl :=
                 Call_Name.P_Referenced_Decl;
            begin
               if Called_Decl.Is_Null then
                  Put_Line ("  --> Resolution unknown");
               else
                  Put_Line
                    ("  --> Declaration: " & Called_Decl.Image & " from " &
                     Called_Decl.Unit.Get_Filename);
               end if;
            end;
         exception
            when Libadalang.Common.Property_Error =>
               Put_Line ("  --> Resolution failed");
         end;
      end loop;
   end Demo_Semantic_P_Properties;

end Examples.Ast;
