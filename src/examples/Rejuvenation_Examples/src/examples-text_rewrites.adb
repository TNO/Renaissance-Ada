with Ada.Text_IO;                use Ada.Text_IO;
with Libadalang.Analysis;        use Libadalang.Analysis;
with Libadalang.Common;          use Libadalang.Common;
with Rejuvenation;               use Rejuvenation;
with Rejuvenation.Factory;       use Rejuvenation.Factory;
with Rejuvenation.Finder;        use Rejuvenation.Finder;
with Rejuvenation.Text_Rewrites; use Rejuvenation.Text_Rewrites;
with Rejuvenation.Utils;         use Rejuvenation.Utils;

package body Examples.Text_Rewrites is

   procedure Demo_Text_Rewrite (Unit : Analysis_Unit);

   procedure Demo (File_Name : String) is
      Unit : constant Analysis_Unit := Open_File (File_Name);
   begin
      Put_Line ("=== Examples of Text_Rewrite =======");
      New_Line;

      Put_Line
        ("--- Example insert comment (before + after Call Statement) -------");
      New_Line;
      Demo_Text_Rewrite (Unit);
      New_Line;
   end Demo;

   procedure Demo_Text_Rewrite (Unit : Analysis_Unit) is
      TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      --  Insert comments before/after call statements
      for Node of Find (Unit.Root, Ada_Call_Stmt) loop
         TR.Prepend
           (Node,
            "--  inserted comment before " & Node.Image & " / " &
            Raw_Signature (Node) & ASCII.CR & ASCII.LF & "   ");
         TR.Append
           (Node,
            ASCII.CR & ASCII.LF & "   --  inserted comment after " &
            Node.Image & " / " & Raw_Signature (Node) & ASCII.CR & ASCII.LF);
      end loop;

      --  Apply all rewrite operations
      Put_Line (TR.ApplyToString);
   end Demo_Text_Rewrite;

end Examples.Text_Rewrites;
