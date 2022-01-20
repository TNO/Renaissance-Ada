with Ada.Text_IO;                 use Ada.Text_IO;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Patterns;       use Rejuvenation.Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;

package body Examples.Finder is

   procedure Demo_Find_Node_Kind (Unit : Analysis_Unit);
   procedure Demo_Find_Pattern (Unit : Analysis_Unit);
   procedure Demo_Find_Sublist_Pattern;

   procedure Demo (File_Name : String) is
      Unit : constant Analysis_Unit := Analyze_File (File_Name);
   begin
      Put_Line ("=== Examples of Finder =======");
      New_Line;

      Put_Line ("--- Example to find node kind -------");
      New_Line;
      Demo_Find_Node_Kind (Unit);
      New_Line;

      Put_Line ("--- Example to find pattern -------");
      New_Line;
      Demo_Find_Pattern (Unit);
      New_Line;

      Put_Line ("--- Example to find sublist pattern -------");
      New_Line;
      Demo_Find_Sublist_Pattern;
      New_Line;
   end Demo;

   procedure Demo_Find_Node_Kind (Unit : Analysis_Unit) is
      Results_Node : constant Node_List.Vector :=
        Find (Unit.Root, Ada_Object_Decl);
   begin
      for Node of Results_Node loop
         Put_Line (Node.Image);
      end loop;
   end Demo_Find_Node_Kind;

   procedure Demo_Find_Pattern (Unit : Analysis_Unit) is
      Results_MP : constant Match_Pattern_List.Vector :=
        Find_Full
          (Unit.Root,
           Make_Pattern ("$S_prefix.$S_method($M_args);", Stmt_Rule));
   begin
      for MP of Results_MP loop
         declare
            Nodes : constant Node_List.Vector := MP.Get_Nodes;
         begin
            Put_Line
              (Nodes.Length'Image & " " & Nodes.Element (1).Image & " " &
               Raw_Signature (MP.Get_Nodes.Element (1)));
         end;
      end loop;
   end Demo_Find_Pattern;

   procedure Demo_Find_Sublist_Pattern is
      Unit : constant Analysis_Unit :=
        Analyze_Fragment
          ("Lock_Mutex(lock_x); x := 2; y := 3; Unlock_Mutex(lock_x);" &
           "Lock_Mutex(lock_y); c := 10; d := 12; Unlock_Mutex(lock_y);",
           Stmts_Rule);
      Results_MP : constant Match_Pattern_List.Vector :=
        Find_Sub_List
          (Unit.Root,
           Make_Pattern
             ("Lock_Mutex($S_mutex); $M_stats; Unlock_Mutex($S_mutex);",
              Stmts_Rule));
   begin
      for MP of Results_MP loop
         Put_Line
           ("Mutex: " & Raw_Signature (MP.Get_Single_As_Node ("$S_mutex")));
         for Stat of MP.Get_Multiple_As_Nodes ("$M_stats") loop
            Put_Line ("* " & Raw_Signature (Stat));
         end loop;
      end loop;
   end Demo_Find_Sublist_Pattern;

end Examples.Finder;
