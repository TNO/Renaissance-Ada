with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with Libadalang.Common;              use Libadalang.Common;
with Rejuvenation;                   use Rejuvenation;
with Rejuvenation.Find_And_Replacer; use Rejuvenation.Find_And_Replacer;
with Rejuvenation.Patterns;          use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory;    use Rejuvenation.Simple_Factory;

procedure Rejuvenation_Find_And_Replace is
   Find_Pattern : constant Pattern :=
     Make_Pattern ("$S_F ($S_Arg1, $S_Arg2);", Call_Stmt_Rule);

   -- Swap arguments - of course resulting in illegal program ;-)
   Replace_Pattern : constant Pattern :=
     Make_Pattern ("$S_F ($S_Arg2, $S_Arg1);", Call_Stmt_Rule);

   Project_Name : constant String := "C:\path\to\your.gpr";
   UFiles : constant Unbounded_Strings.Vector :=
     Get_Ada_Source_Files_From_Project (Project_Name);
begin
   for UFile of UFiles loop
      declare
         File  : constant String  := To_String (UFile);
      begin
         if Find_And_Replace (File, Find_Pattern, Replace_Pattern) then
            Put_Line ("Changed - " & File);
         end if;
      end;
   end loop;
end Rejuvenation_Find_And_Replace;
