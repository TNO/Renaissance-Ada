with Ada.Text_IO;                 use Ada.Text_IO;
with AUnit.Assertions;            use AUnit.Assertions;
--  with GNAT.Source_Info;            use GNAT.Source_Info;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;
--  with Rewriters_Node_Utils;        use Rewriters_Node_Utils;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Ada_Node_Interval; use Ada_Node_Interval;
with Intervals;         use Intervals;

package body Test_Node_Interval is

   procedure Assert_Equivalent_Parent (P : Ada_Node);
   --  Parent is textual equivalent with its children when
   --  it has exactly one textual child
   --  Children are NOT textual when
   --  1. Child is Null node
   --  2. Child is an absent node,
   --     like Ada_Constant_Absent and Ada_Not_Null_Absent
   --  3. Child is an empty list
   procedure Assert_Equivalent_Parent (P : Ada_Node) is
      Count : Natural := 0;
   begin
      for Child of P.Children loop
         if not Child.Is_Null then
            case Child.Kind is
               when Ada_Abort_Absent | Ada_Abstract_Absent |
                 Ada_Aliased_Absent | Ada_All_Absent | Ada_Constant_Absent |
                 Ada_Limited_Absent | Ada_Not_Null_Absent |
                 Ada_Private_Absent | Ada_Protected_Absent |
                 Ada_Reverse_Absent | Ada_Synchronized_Absent |
                 Ada_Tagged_Absent | Ada_Until_Absent |
                 Ada_With_Private_Absent =>
                  null;
               when Ada_Ada_List =>
                  if Child.Children_Count > 0 then
                     Count := Count + 1;
                  end if;
               when others =>
                  Count := Count + 1;
            end case;
         end if;
      end loop;

      if Count /= 1 then
         Assert
           (False,
            "Parent is not textual equivalent with its child" & ASCII.LF &
            P.Kind'Image & ": " & Raw_Signature (P) & ASCII.LF & Count'Image);
      end if;
   end Assert_Equivalent_Parent;

   procedure Assert_Equivalent_Parents (N : Ada_Node; PS : Ada_Node_Array);
   procedure Assert_Equivalent_Parents (N : Ada_Node; PS : Ada_Node_Array) is
   begin
      for P of PS loop
         Assert_Equivalent_Parent (P);
         if N = P then
            exit;
         end if;
      end loop;
   end Assert_Equivalent_Parents;

   procedure Assert_Equivalent_Node (N_A, N_B : Ada_Node);
   procedure Assert_Equivalent_Node (N_A, N_B : Ada_Node) is
   begin
      if N_A /= N_B then
         declare
            PS_A : constant Ada_Node_Array := N_A.Parents (False);
         begin
            if (for some P_A of PS_A => N_B = P_A) then
               Assert_Equivalent_Parents (N_B, PS_A);
            else
               declare
                  PS_B : constant Ada_Node_Array := N_B.Parents (False);
               begin
                  if (for some P_B of PS_B => N_A = P_B) then
                     Assert_Equivalent_Parents (N_A, PS_B);
                  else
                     Assert
                       (False,
                        "Nodes are not textual equivalent" & ASCII.LF &
                        N_A.Kind'Image & ": " & Raw_Signature (N_A) &
                        ASCII.LF & "and" & ASCII.LF & N_B.Kind'Image & ": " &
                        Raw_Signature (N_B));
                  end if;
               end;
            end if;
         end;
      end if;
   end Assert_Equivalent_Node;

   procedure Assert_Contain_Node (N_A, N_B : Ada_Node);
   --  Sub interval only
   procedure Assert_Contain_Node (N_A, N_B : Ada_Node) is
   begin
      if (for all P_B of N_B.Parents (False) => P_B /= N_A) then
         Assert
           (False,
            "Unexpectedly" & ASCII.LF & N_A.Kind'Image & ": " &
            Raw_Signature (N_A) & ASCII.LF & "does not contain" & ASCII.LF &
            N_B.Kind'Image & ": " & Raw_Signature (N_B));
      end if;
   end Assert_Contain_Node;

   procedure Assert_NonOverlapping_Nodes
     (N_1, N_2 : Ada_Node; G_I : Get_Interval);
   procedure Assert_NonOverlapping_Nodes
     (N_1, N_2 : Ada_Node; G_I : Get_Interval)
   is
      I1 : constant Interval := G_I (N_1);
      I2 : constant Interval := G_I (N_2);
   begin
      if not Is_Empty (I1) and then not Is_Empty (I2) then
         if I1 = I2 then
            Assert_Equivalent_Node (N_1, N_2);
         elsif Contains (I1, I2) then
            Assert_Contain_Node (N_1, N_2);
         elsif Contains (I2, I1) then
            Assert_Contain_Node (N_2, N_1);
         elsif Overlaps (I1, I2) then
            Assert
              (False,
               "Unexpected partial overlap between" & ASCII.LF &
               N_1.Kind'Image & ": " & Raw_Signature (N_1) & ASCII.LF & "and" &
               ASCII.LF & N_2.Kind'Image & ": " & Raw_Signature (N_2));
         end if;
      end if;
   end Assert_NonOverlapping_Nodes;

   procedure Assert_Unit (Unit : Analysis_Unit; G_I : Get_Interval);
   procedure Assert_Unit (Unit : Analysis_Unit; G_I : Get_Interval) is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
         pragma Unreferenced (Node);
      begin
         return True;
      end Predicate;

      All_Nodes : constant Node_List.Vector :=
        Find (Unit.Root, Predicate'Access);

   begin
      Put_Line ("===== " & Unit.Get_Filename & " =====");
      for I1 in 1 .. Integer (All_Nodes.Length) loop
         declare
            E1 : constant Ada_Node := All_Nodes.Element (I1);
         begin
            for I2 in 1 .. I1 - 1 loop
               declare
                  E2 : constant Ada_Node := All_Nodes.Element (I2);
               begin
                  Assert_NonOverlapping_Nodes (E1, E2, G_I);
               end;
            end loop;
         end;
      end loop;
   end Assert_Unit;

   function To_String
     (Token : Token_Reference; Unit : Analysis_Unit) return String;
   function To_String
     (Token : Token_Reference; Unit : Analysis_Unit) return String
   is
      T_T : constant String     := Encode (Text (Token), Unit.Get_Charset);
      T_K : constant Token_Kind := Kind (Data (Token));
   begin
      return Token_Kind'Image (T_K) & " `" & T_T & "`";
   end To_String;

   procedure Test_Tokens (T : in out Test_Case'Class);
   procedure Test_Tokens (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Unit : constant Analysis_Unit :=
        Analyze_Fragment
          ("  A  " & ASCII.LF & "--$mark start" & ASCII.LF, Expr_Rule);
      Node : constant Ada_Node := Unit.Root;
   begin
      for Token in Node.Token_Range loop
         Put_Line (To_String (Token, Unit));
      end loop;
   end Test_Tokens;

   function Node_To_Interval is new Get_Interval_With_Trivia
     (No_Trivia, No_Trivia);
   procedure Test_Independent_Nodes (T : in out Test_Case'Class);
   procedure Test_Independent_Nodes (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      --  Unit : constant Analysis_Unit :=
      --    Analyze_File ("src/" & GNAT.Source_Info.File);
      Units : constant Analysis_Units.Vector :=
        Analyze_Project
          ("C:\path\to\Renaissance-Ada\src\libraries\Rewriters\rewriters.gpr");
   begin
      for Unit of Units loop
         Assert_Unit (Unit, Node_To_Interval'Access);
      end loop;
   end Test_Independent_Nodes;

   --
   --  procedure Test_First_Node (T : in out Test_Case'Class);
   --  procedure Test_First_Node (T : in out Test_Case'Class) is
   --     pragma Unreferenced (T);
   --
   --     Unit   : constant Analysis_Unit :=
   --       Analyze_Fragment ("  A  " & ASCII.LF & "--$mark start" & ASCII.LF,
   --                         Expr_Rule);
   --     Node   : constant Ada_Node        := Unit.Root;
   --     Actual : constant Token_Reference :=
   --       Last_Token_Of_Preceeding_Node (Node);
   --     Expected : constant Token_Reference := No_Token;
   --  begin
   --     Assert (Node.Parent.Is_Null, "Node without parent expected");
   --     Assert
   --       (Node.Previous_Sibling.Is_Null,
   --        "Node without previous sibling expected");
   --     Assert (Actual = Expected, "Actual differs from Expected");
   --  end Test_First_Node;
   --
   --  procedure Test_Last_Node (T : in out Test_Case'Class);
   --  procedure Test_Last_Node (T : in out Test_Case'Class) is
   --     pragma Unreferenced (T);
   --  begin
   --     Assert (False, "TODO");
   --  end Test_Last_Node;
   --
   --  procedure Test_If_Nodes (T : in out Test_Case'Class);
   --  procedure Test_If_Nodes (T : in out Test_Case'Class) is
   --     pragma Unreferenced (T);
   --
   --     Unit : constant Analysis_Unit :=
   --       Analyze_Fragment ("if C then X; else Y; end if;", If_Stmt_Rule);
   --     Node : constant Ada_Node := Unit.Root;
   --  begin
   --     Assert (Node.Kind = Ada_If_Stmt, "Node kind is not If_Stmt");
   --     declare
   --        I_S : constant If_Stmt := Node.As_If_Stmt;
   --     begin
   --        Node.Print;
   --        Assert
   --          (Node.Children_Count = 4,
   --           "Unexpected number of children " & Node.Children_Count'Image);
   --        --  include empty alternatives list
   --        Assert
   --          (Last_Token_Of_Preceeding_Node (I_S.F_Then_Stmts) =
   --           I_S.F_Cond_Expr.Token_End,
   --           "Preceeding Then Stmt failed");
   --        Assert
   --          (Last_Token_Of_Preceeding_Node (I_S.F_Else_Stmts) =
   --           I_S.F_Then_Stmts.Token_End,
   --           "Preceeding Else Stmt failed" & ASCII.LF & "Actual   " &
   --           To_String
   --             (Last_Token_Of_Preceeding_Node (I_S.F_Else_Stmts), Unit) &
   --           ASCII.LF & "Expected " &
   --           To_String (I_S.F_Then_Stmts.Token_End, Unit));
   --     end;
   --  end Test_If_Nodes;
   --
   --  procedure Test_ElsIf_Nodes (T : in out Test_Case'Class);
   --  procedure Test_ElsIf_Nodes (T : in out Test_Case'Class) is
   --     pragma Unreferenced (T);
   --
   --     Unit : constant Analysis_Unit :=
   --       Analyze_Fragment
   --         ("if C1 then X; elsif C2 then Y; else Z; end if;", If_Stmt_Rule);
   --     Node : constant Ada_Node := Unit.Root;
   --  begin
   --     Assert (Node.Kind = Ada_If_Stmt, "Node kind is not If_Stmt");
   --     declare
   --        I_S : constant If_Stmt := Node.As_If_Stmt;
   --     begin
   --        Assert
   --          (Node.Children_Count = 4,
   --           "Unexpected number of children " & Node.Children_Count'Image);
   --        Assert
   --          (Last_Token_Of_Preceeding_Node (I_S.F_Then_Stmts) =
   --           I_S.F_Cond_Expr.Token_End,
   --           "Preceeding Then Stmt failed");
   --        Assert
   --          (Last_Token_Of_Preceeding_Node (I_S.F_Alternatives) =
   --           I_S.F_Then_Stmts.Token_End,
   --           "Preceeding Alternatives Stmt failed");
   --        Assert
   --          (Last_Token_Of_Preceeding_Node (I_S.F_Else_Stmts) =
   --           I_S.F_Alternatives.Token_End,
   --           "Preceeding Else Stmt failed");
   --     end;
   --  end Test_ElsIf_Nodes;

--  Test plumbing

   overriding function Name
     (T : Node_Interval_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Node_Boundaries");
   end Name;

   overriding procedure Register_Tests (T : in out Node_Interval_Test_Case) is
   begin

      Registration.Register_Routine
        (T, Test_Independent_Nodes'Access, "Are Nodes Independent");
      Registration.Register_Routine (T, Test_Tokens'Access, "Test Tokens");
      --  Registration.Register_Routine
      --    (T, Test_First_Node'Access, "First Node");
      --  Registration.Register_Routine (T, Test_Last_Node'Access, "Last Node");
      --  Registration.Register_Routine
      --    (T, Test_If_Nodes'Access, "Nodes in if statement");
      --  Registration.Register_Routine
      --    (T, Test_ElsIf_Nodes'Access, "Nodes in if elsif statement");
   end Register_Tests;

end Test_Node_Interval;
