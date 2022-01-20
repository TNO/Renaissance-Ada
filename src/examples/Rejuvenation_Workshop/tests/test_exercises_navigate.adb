--  very excotic with and use clauses for demonstration purposes
with Ada.Assertions;        use Ada.Assertions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT
  . --  with comment
Source_Info; use GNAT
    .  --  and even more comment
Source_Info;
with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis, Libadalang.Common;
use Libadalang.Analysis, Libadalang.Common;
with Shared, Rejuvenation.Simple_Factory;
use Rejuvenation.Simple_Factory, Shared;

package body Test_Exercises_Navigate is

   --  Some examples of With Use clauses and what is reported:
   --  * With X; Use X;        -> X is reported
   --  * With X, Y; Use X, Y;  -> X and Y are reported
   --  * With X, Y; Use Y, X;  -> X and Y are reported
   --  * With X, Y; Use X;     -> X is reported
   --  * With X, Y; Use Y;     -> Y is reported
   --  * With X.Y; Use X, X.Y; -> X.Y is reported
   --  * With X.Y; Use X.Y, X; -> X.Y is reported
   procedure Test_LibAdaLang_WithUse_Packages (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_WithUse_Packages (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      function Canonical_Name (N : Libadalang.Analysis.Name) return String;
      function Canonical_Name (N : Libadalang.Analysis.Name) return String is
         function Image (UTT : Unbounded_Text_Type) return String;
         function Image (UTT : Unbounded_Text_Type) return String is
         begin
            return Image (To_Text (UTT));
         end Image;

         function Join
           (UTTA : Unbounded_Text_Type_Array; Separator : String)
            return String;
         function Join
           (UTTA : Unbounded_Text_Type_Array; Separator : String) return String
         is
         begin
            if UTTA'Length = 0 then
               return "";
            else
               declare
                  Return_Value : Unbounded_String;
               begin
                  for Index in UTTA'First .. Positive'Pred (UTTA'Last) loop
                     Append (Return_Value, Image (UTTA (Index)) & Separator);
                  end loop;
                  Append (Return_Value, Image (UTTA (UTTA'Last)));
                  return To_String (Return_Value);
               end;
            end if;
         end Join;

      begin
         return Join (N.P_As_Symbol_Array, ".");
      end Canonical_Name;

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_With_Clause then
            declare
               WC   : constant With_Clause := Node.As_With_Clause;
               Next : constant Ada_Node    := WC.Next_Sibling;
            begin
               if not Next.Is_Null and then Next.Kind = Ada_Use_Package_Clause
               then
                  declare
                     UC : constant Use_Package_Clause :=
                       WC.Next_Sibling.As_Use_Package_Clause;
                  begin
                     for WC_Package of WC.F_Packages loop
                        declare
                           WC_PN : constant Libadalang.Analysis.Name :=
                             WC_Package.As_Name;
                        begin
                           for UC_Package of UC.F_Packages loop
                              declare
                                 UC_PN : constant Libadalang.Analysis.Name :=
                                   UC_Package.As_Name;
                              begin
                                 if P_Name_Matches (WC_PN, UC_PN) then
                                    Put_Line
                                      ("Found consecutive 'with' " &
                                       "and 'use' of " &
                                       Canonical_Name (WC_PN));
                                 end if;
                              end;
                           end loop;
                        end;
                     end loop;
                  end;
               end if;
            end;
         end if;
         return Into;
      end Process_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("tests/" & GNAT.Source_Info.File);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_WithUse_Packages;

   procedure Test_LibAdaLang_Public_Subp_Definition_With_3_Parameters
     (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Public_Subp_Definition_With_3_Parameters
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Ada_Private_Part =>
               --  Public Declarations only!
               return Over;
            when Ada_Subp_Spec =>
               declare
                  SS : constant Subp_Spec := Node.As_Subp_Spec;
               begin
                  if Is_Part_Of_Subp_Def (SS)
                    and then Nr_Of_Parameters (SS) = 3
                  then
                     Put_Line ("Found " & Image (SS.F_Subp_Name.Text));
                  end if;
               end;
               return Into;
            when others =>
               return Into;
         end case;
      end Process_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("src/count_subprogram.ads");
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_Public_Subp_Definition_With_3_Parameters;

   procedure Test_LibAdaLang_Used_External_Declarations
     (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Used_External_Declarations
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Identifier then
            declare
               Id : constant Identifier := Node.As_Identifier;
               RD : constant Refd_Def   := Id.P_Failsafe_Referenced_Def_Name;
            begin
               case Kind (RD) is
                  when Precise =>
                     if Node.Unit /= Def_Name (RD).Unit then
                        Put_Line
                          (Image (Node.Full_Sloc_Image) & Image (Node.Text) &
                           " references to " & Image (Def_Name (RD).Text) &
                           " at " & Image (Def_Name (RD).Full_Sloc_Image));
                     end if;
                  when Error =>
                     null;
               --  Put_Line (Image (Node.Full_Sloc_Image) & Image (Node.Text) &
                     --              " doesn't reference anything");
                  when others =>
                     Assert
                       (Check   => False,
                        Message =>
                          "Assumption that project can be " &
                          "correct compiled seems violated: " &
                          Image (Node.Full_Sloc_Image) & Image (Node.Text) &
                          " results in " & Kind (RD)'Image);
               end case;
            exception
               when others =>
                  null;
            end;
         end if;
         return Into;
      end Process_Node;

      Filename         : constant String := "tests/" & GNAT.Source_Info.File;
      Project_Filename : constant String        := "test_driver.gpr";
      Unit             : constant Analysis_Unit :=
        Analyze_File_In_Project (Filename, Project_Filename);
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_Used_External_Declarations;

   procedure Test_LibAdaLang_Find_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class);
   procedure Test_LibAdaLang_Find_Assign_Condition_In_If_Statement
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Found_Matches : Natural := 0;

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_If_Stmt then
            declare
               ifStmt : constant If_Stmt := Node.As_If_Stmt;
            begin
               if not ifStmt.F_Then_Stmts.Is_Null
                 and then not ifStmt.F_Else_Stmts.Is_Null
                 and then ifStmt.F_Alternatives.Children_Count = 0
                 and then ifStmt.F_Then_Stmts.Children_Count = 1
                 and then ifStmt.F_Else_Stmts.Children_Count = 1
               then
                  declare
                     ThenNode : constant Ada_Node :=
                       ifStmt.F_Then_Stmts.Child
                         (ifStmt.F_Then_Stmts.First_Child_Index);
                     ElseNode : constant Ada_Node :=
                       ifStmt.F_Else_Stmts.Child
                         (ifStmt.F_Else_Stmts.First_Child_Index);
                  begin
                     if ThenNode.Kind = Ada_Assign_Stmt
                       and then ElseNode.Kind = Ada_Assign_Stmt
                       and then ThenNode.As_Assign_Stmt.F_Dest.Text =
                         ElseNode.As_Assign_Stmt.F_Dest.Text
                       and then ThenNode.As_Assign_Stmt.F_Expr.Kind =
                         Ada_Identifier
                       and then Ada.Strings.Equal_Case_Insensitive
                         (Image
                            (ThenNode.As_Assign_Stmt.F_Expr.As_Identifier
                               .Text),
                          "True")
                       and then ElseNode.As_Assign_Stmt.F_Expr.Kind =
                         Ada_Identifier
                       and then Ada.Strings.Equal_Case_Insensitive
                         (Image
                            (ElseNode.As_Assign_Stmt.F_Expr.As_Identifier
                               .Text),
                          "False")
                     then
                        Found_Matches := Found_Matches + 1;
                        Put_Line
                          (Image (ifStmt.Full_Sloc_Image) &
                           Image (ThenNode.As_Assign_Stmt.F_Dest.Text) &
                           " := " & Image (ifStmt.F_Cond_Expr.Text) & ";");
                     end if;
                  end;
               end if;
            end;
         end if;
         return Into;
      end Process_Node;

      Unit : constant Analysis_Unit :=
        Analyze_File ("src/assignmentbyifexamples.adb");
   begin
      Put_Line ("Begin - " & Enclosing_Entity);
      Unit.Root.Traverse (Process_Node'Access);
      Assert
        (Found_Matches = 2,
         "Two instances in unit expected, got " & Found_Matches'Image);
      Put_Line ("Done - " & Enclosing_Entity);
   end Test_LibAdaLang_Find_Assign_Condition_In_If_Statement;

   --  Test plumbing

   overriding function Name
     (T : Exercise_Navigate_Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Exercises Navigate");
   end Name;

   overriding procedure Register_Tests (T : in out Exercise_Navigate_Test_Case)
   is
   begin
      Registration.Register_Routine
        (T, Test_LibAdaLang_WithUse_Packages'Access,
         "Use LibAdaLang to packages that are included and used (with-use)");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Public_Subp_Definition_With_3_Parameters'Access,
         "Use LibAdaLang to find Public Subprograms with 3 Parameters");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Used_External_Declarations'Access,
         "Use LibAdaLang to find All Used External Declarations");
      Registration.Register_Routine
        (T, Test_LibAdaLang_Find_Assign_Condition_In_If_Statement'Access,
         "Use LibAdaLang to find assignment of " &
         "condition to variable using if statement. " & "Pattern 1");
   end Register_Tests;

end Test_Exercises_Navigate;
