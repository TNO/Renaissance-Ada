with Ada.Text_IO;                 use Ada.Text_IO;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Factory;        use Rejuvenation.Factory;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Examples.Factory is

   procedure Demo_Parse_Full_Text;
   procedure Demo_Parse_Partial_Text;
   procedure Demo_Parse_File_Outside_Project_Context (File_Name : String);
   procedure Demo_Parse_File_Inside_Project_Context
     (Project_Name : String; File_Name : String);

   procedure Demo (Project_Name : String; File_Name : String) is
   begin
      Put_Line ("=== Examples of Factory =======");
      New_Line;

      Put_Line ("--- Example of parsing full text -------");
      New_Line;
      Demo_Parse_Full_Text;
      New_Line;

      Put_Line ("--- Example of parsing partial text -------");
      New_Line;
      Demo_Parse_Partial_Text;
      New_Line;

      Put_Line ("--- Example of parsing file outside project context -------");
      New_Line;
      Demo_Parse_File_Outside_Project_Context (File_Name);
      New_Line;

      Put_Line ("--- Example of parsing file inside project context -------");
      New_Line;
      Demo_Parse_File_Inside_Project_Context (Project_Name, File_Name);
      New_Line;
   end Demo;

   procedure Demo_Parse_Full_Text is
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("procedure Test is begin New_Line; end;");
   begin
      Unit.Print; -- Show the node's internal structure
   end Demo_Parse_Full_Text;

   procedure Demo_Parse_Partial_Text is
      Unit : constant Analysis_Unit := Analyze_Fragment ("x := 4;", Stmt_Rule);
   begin
      Unit.Print; -- Show the node's internal structure
   end Demo_Parse_Partial_Text;

   procedure Demo_Parse_File_Outside_Project_Context (File_Name : String) is
      Unit : constant Analysis_Unit := Open_File (File_Name);
   begin
      Unit.Print; -- Show the node's internal structure
   end Demo_Parse_File_Outside_Project_Context;

   procedure Demo_Parse_File_Inside_Project_Context
     (Project_Name : String; File_Name : String)
   is
      Context : constant Project_Context := Open_Project (Project_Name);
      Unit    : constant Analysis_Unit   := Open_File (File_Name, Context);
   begin
      Unit.Print; -- Show the node's internal structure
   end Demo_Parse_File_Inside_Project_Context;

end Examples.Factory;
