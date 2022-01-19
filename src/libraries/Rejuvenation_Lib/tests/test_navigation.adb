with AUnit.Assertions;            use AUnit.Assertions;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Test_Navigation is

   --  Helper functions

   function Is_A_Node (Node : Ada_Node'Class) return Boolean;
   function Is_A_Node (Node : Ada_Node'Class) return Boolean is
      pragma Unreferenced (Node);
   begin
      return True;
   end Is_A_Node;

   --  Test Functions

   procedure Test_Ancestor_Self (T : in out Test_Case'Class);
   procedure Test_Ancestor_Self (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Ancestor_Self (Unit : Analysis_Unit);
      procedure Test_Ancestor_Self (Unit : Analysis_Unit) is
         Nodes : constant Node_List.Vector :=
           Find (Unit.Root, Is_A_Node'Access);
      begin
         for Node of Nodes loop
            Assert
              (Condition => not Is_Ancestor (Node, Node),
               Message   => "Node should not be an ancestor of itself");
         end loop;
      end Test_Ancestor_Self;

      Stmts_String : constant String        := "x:=y+z;k:=l-m;";
      Unit         : constant Analysis_Unit :=
        Analyze_Fragment (Stmts_String, Stmts_Rule);
   begin
      Test_Ancestor_Self (Unit);
   end Test_Ancestor_Self;

   procedure Test_Ancestor_Root (T : in out Test_Case'Class);
   procedure Test_Ancestor_Root (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Ancestor_Root (Unit : Analysis_Unit);
      procedure Test_Ancestor_Root (Unit : Analysis_Unit) is
         Nodes : constant Node_List.Vector :=
           Find (Unit.Root, Is_A_Node'Access);
      begin
         for Node of Nodes loop
            Assert
              (Condition =>
                 Is_Ancestor (Unit.Root, Node) = (Node /= Unit.Root),
               Message =>
                 "Root node is ancestor of all nodes " &
                 "except of itself (the Root node)");
            Assert
              (Condition => not Is_Ancestor (Node, Unit.Root),
               Message   => "Node is never an ancestor of the Root node");
         end loop;
      end Test_Ancestor_Root;

      Stmts_String : constant String        := "x:=y+z;k:=l-m;";
      Unit         : constant Analysis_Unit :=
        Analyze_Fragment (Stmts_String, Stmts_Rule);
   begin
      Test_Ancestor_Root (Unit);
   end Test_Ancestor_Root;

   procedure Test_Reflexive_Ancestor_Self (T : in out Test_Case'Class);
   procedure Test_Reflexive_Ancestor_Self (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Reflexive_Ancestor_Self (Unit : Analysis_Unit);
      procedure Test_Reflexive_Ancestor_Self (Unit : Analysis_Unit) is
         Nodes : constant Node_List.Vector :=
           Find (Unit.Root, Is_A_Node'Access);
      begin
         for Node of Nodes loop
            Assert
              (Condition => Is_Reflexive_Ancestor (Node, Node),
               Message   => "Node should be a reflexive ancestor of itself");
         end loop;
      end Test_Reflexive_Ancestor_Self;

      Stmts_String : constant String        := "x:=y+z;k:=l-m;";
      Unit         : constant Analysis_Unit :=
        Analyze_Fragment (Stmts_String, Stmts_Rule);
   begin
      Test_Reflexive_Ancestor_Self (Unit);
   end Test_Reflexive_Ancestor_Self;

   procedure Test_Reflexive_Ancestor_Root (T : in out Test_Case'Class);
   procedure Test_Reflexive_Ancestor_Root (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      procedure Test_Reflexive_Ancestor_Root (Unit : Analysis_Unit);
      procedure Test_Reflexive_Ancestor_Root (Unit : Analysis_Unit) is
         Nodes : constant Node_List.Vector :=
           Find (Unit.Root, Is_A_Node'Access);
      begin
         for Node of Nodes loop
            Assert
              (Condition => Is_Reflexive_Ancestor (Unit.Root, Node),
               Message => "Root should be a reflexive ancestor of all nodes");
            Assert
              (Condition =>
                 Is_Reflexive_Ancestor (Node, Unit.Root) = (Node = Unit.Root),
               Message =>
                 "Node is only reflexive ancestor of Root when it is Root");
         end loop;
      end Test_Reflexive_Ancestor_Root;

      Stmts_String : constant String        := "x:=y+z;k:=l-m;";
      Unit         : constant Analysis_Unit :=
        Analyze_Fragment (Stmts_String, Stmts_Rule);
   begin
      Test_Reflexive_Ancestor_Root (Unit);
   end Test_Reflexive_Ancestor_Root;

   --  Test plumbing

   overriding function Name
     (T : Navigation_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Navigation");
   end Name;

   overriding procedure Register_Tests (T : in out Navigation_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Ancestor_Self'Access, "Ancestor - Self");
      Registration.Register_Routine
        (T, Test_Ancestor_Root'Access, "Ancestor - Root");
      Registration.Register_Routine
        (T, Test_Reflexive_Ancestor_Self'Access, "Reflexive Ancestor - Self");
      Registration.Register_Routine
        (T, Test_Reflexive_Ancestor_Root'Access, "Reflexive Ancestor - Root");
   end Register_Tests;

end Test_Navigation;
