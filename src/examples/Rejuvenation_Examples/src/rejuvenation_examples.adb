with Examples.Ast;
with Examples.Factory;
with Examples.Finder;
with Examples.Match_Patterns;
with Examples.Navigation;
with Examples.Text_Rewrites;
with Examples.Visitor;

procedure Rejuvenation_Examples is
   Root_Folder  : constant String := "";
   Project_Name : constant String := Root_Folder & "rejuvenation_examples.gpr";
   File_Name    : constant String :=
     Root_Folder & "src/rejuvenation_examples.adb";
begin

   -- Parsing --------
   Examples.Factory.Demo (Project_Name, File_Name);

   -- Code analysis --------
   Examples.Ast.Demo (Project_Name, File_Name);
   Examples.Visitor.Demo (File_Name);
   Examples.Match_Patterns.Demo;
   Examples.Finder.Demo (File_Name);
   Examples.Navigation.Demo (File_Name);

   -- Code manipulation --------
   Examples.Text_Rewrites.Demo (File_Name);

end Rejuvenation_Examples;
