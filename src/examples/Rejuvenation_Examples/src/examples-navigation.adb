with Ada.Text_IO;             use Ada.Text_IO;
with Libadalang.Analysis;     use Libadalang.Analysis;
with Libadalang.Common;       use Libadalang.Common;
with Rejuvenation;            use Rejuvenation;
with Rejuvenation.Factory;    use Rejuvenation.Factory;
with Rejuvenation.Finder;     use Rejuvenation.Finder;
with Rejuvenation.Navigation; use Rejuvenation.Navigation;

package body Examples.Navigation is

   procedure Demo_Navigate_Node (Unit : Analysis_Unit);

   procedure Demo (File_Name : String) is
      Unit : constant Analysis_Unit := Open_File (File_Name);
   begin
      Put_Line ("=== Examples of Navigation =======");
      New_Line;

      Put_Line ("--- Example to navigate between nodes -------");
      New_Line;
      Demo_Navigate_Node (Unit);
      New_Line;
   end Demo;

   procedure Demo_Navigate_Node (Unit : Analysis_Unit) is
      Results_Node : constant Node_List.Vector :=
        Find (Unit.Root, Ada_Call_Expr);
   begin
      for Node of Results_Node loop
         declare
            ObjNode : constant Ada_Node :=
              Get_Ancestor_Of_Type (Node, Ada_Call_Stmt);
         begin
            if ObjNode /= No_Ada_Node then
               Put_Line
                 ("Call_Expr " & Node.Image & " inside Call_Stmt " &
                  ObjNode.Image);
            end if;
         end;
      end loop;
   end Demo_Navigate_Node;

end Examples.Navigation;
