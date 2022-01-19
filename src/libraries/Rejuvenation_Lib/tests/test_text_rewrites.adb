with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;                         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;                     use Ada.Strings.Unbounded;
with AUnit.Assertions;                          use AUnit.Assertions;
with Langkit_Support.Slocs;                     use Langkit_Support.Slocs;
with Langkit_Support.Text;                      use Langkit_Support.Text;
with Libadalang.Analysis;                       use Libadalang.Analysis;
with Libadalang.Common;                         use Libadalang.Common;
with Rejuvenation;                              use Rejuvenation;
with Rejuvenation.Finder;                       use Rejuvenation.Finder;
with Rejuvenation.Node_Locations;               use Rejuvenation.Node_Locations;
with Rejuvenation.Simple_Factory;               use Rejuvenation.Simple_Factory;
with Rejuvenation.Text_Rewrites;                use Rejuvenation.Text_Rewrites;

with Make_Ada;                                  use Make_Ada;

package body Test_Text_Rewrites is

   --  Helper data structures
   type Unbounded_String_Array is array (Positive range <>) of Unbounded_String;

   Strings : constant Unbounded_String_Array (1 .. 4) :=
     (To_Unbounded_String (""),
      To_Unbounded_String ("X"),
      To_Unbounded_String ("xx"),
      To_Unbounded_String ("hfjkhfasj"));

   Identifiers : constant Unbounded_String_Array (1 .. 3) :=
     Strings (2 .. 4);

   --  Helper functions

   procedure Assert_Unique_Nodes (Nodes : Node_List.Vector) is
   begin
      for Index1 in Nodes.First_Index .. Nodes.Last_Index loop
         declare
            Node1 : constant Ada_Node'Class := Nodes.Element (Index1);
            Text1 : constant String         := Image (Node1.Text);
         begin
            for Index2 in Index1 + 1 .. Nodes.Last_Index loop
               declare
                  Node2 : constant Ada_Node'Class := Nodes.Element (Index2);
                  Text2 : constant String         := Image (Node2.Text);
               begin
                  Assert (Condition => Text1 /= Text2,
                          Message => "Precondition violated, same node at " &
                            Image (Node1.Sloc_Range) & " and " &
                            Image (Node2.Sloc_Range));
               end;
            end loop;
         end;
      end loop;
   end Assert_Unique_Nodes;

   procedure Assert_Unique_Insert (Nodes : Node_List.Vector; Insert : String) is
   begin
      for Node of Nodes loop
         declare
            Text : constant String := Image (Node.Text);
         begin
            Assert (Condition => Text /= Insert,
                    Message => "Insert not unique, node at " &
                      Image (Node.Sloc_Range) & " same as insert '" &
                      Insert & "'");
         end;
      end loop;
   end Assert_Unique_Insert;

   procedure Assert_Unique_Strings (a, b : String) is
   begin
      Assert (Condition => a /= b,
              Message => "Strings not unique, a '" & a & "' same as b '" & b & "'");
   end Assert_Unique_Strings;

   function Is_A_Node (Node : Ada_Node'Class) return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return True;
   end Is_A_Node;

   --  Test Functions

   procedure Test_None
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_None
        (Unit : Analysis_Unit)
      is
         TR : constant Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      begin
         Assert (TR.ApplyToString,
                 Encode (Unit.Text, Unit.Get_Charset),
                 "Text Rewrite with no operations should be original text " & Unit.Get_Filename
                );
      end Test_None;

      Stmt_String : constant String        := "a := b + c;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmt_String, Stmt_Rule);
   begin
      Test_None (Unit);
   end Test_None;

   procedure Test_Replace_Root
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Replace_Root
        (Unit : Analysis_Unit)
      is
         Replacement : constant String            := "anything goes";
         TR          :          Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      begin
         TR.Replace (Unit.Root, Replacement);
         Assert (TR.ApplyToString,
                 Replacement,
                 "Replace Root by Replacement should be Replacement");
      end Test_Replace_Root;

      Stmt_String : constant String        := "a := b + c;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmt_String, Stmt_Rule);
   begin
      Test_Replace_Root (Unit);
   end Test_Replace_Root;

   procedure Test_Prepend
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Stmt_String : constant String        := "x:=y+z;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmt_String, Stmt_Rule);
      Prepend     : constant String        := "p";

      procedure Test_Prepend (Node : Ada_Node)
      is
         TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      begin
         TR.Prepend (Node, Prepend);
         declare
            Result        : constant String := TR.ApplyToString;
            Node_String   : constant String := Image (Node.Text);
            Index_Node    : constant Natural := Index (Source => Result,  Pattern => Node_String);
            Index_Prepend : constant Natural := Index (Source => Result,  Pattern => Prepend);
         begin
            Assert (Index_Prepend + Prepend'Length <= Index_Node, "Prepend should be before node");
         end;
      end Test_Prepend;

      Nodes : constant Node_List.Vector := Find (Unit.Root, Is_A_Node'Access);
   begin
      Assert_Unique_Nodes (Nodes);
      Assert_Unique_Insert (Nodes, Prepend);

      for Node of Nodes loop
         Test_Prepend (Node);
      end loop;
   end Test_Prepend;

   procedure Test_Append
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Stmt_String : constant String        := "x:=y+z;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmt_String, Stmt_Rule);
      Append      : constant String        := "a";

      procedure Test_Append (Node : Ada_Node)
      is
         TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      begin
         TR.Append (Node, Append);
         declare
            Result       : constant String := TR.ApplyToString;
            Node_String  : constant String := Image (Node.Text);
            Index_Node   : constant Natural := Index (Source => Result,  Pattern => Node_String);
            Index_Append : constant Natural := Index (Source => Result,  Pattern => Append);
         begin
            Assert (Index_Node + Node_String'Length <= Index_Append,
                    "Append should be after node");
         end;
      end Test_Append;

      Nodes : constant Node_List.Vector := Find (Unit.Root, Is_A_Node'Access);
   begin
      Assert_Unique_Nodes (Nodes);
      Assert_Unique_Insert (Nodes, Append);

      for Node of Nodes loop
         Test_Append (Node);
      end loop;
   end Test_Append;

   procedure Test_Prepend_Replace
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      for UExpr_String of Identifiers loop
         declare
            Expr_String : constant String := To_String (UExpr_String);
            Unit        : constant Analysis_Unit := Analyze_Fragment (Expr_String, Expr_Rule);
         begin
            for UPrepend of Strings loop
               declare
                  Prepend      : constant String := To_String (UPrepend);
               begin
                  for UReplace of Strings loop
                     declare
                        Replace      : constant String := To_String (UReplace);
                     begin
                        --  Prepend First, Replace Second
                        declare
                           TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
                        begin
                           TR.Prepend (Unit.Root, Prepend);
                           TR.Replace (Unit.Root, Replace);
                           declare
                              Result       : constant String := TR.ApplyToString;
                           begin
                              Assert (Result = Prepend & Replace,
                                      "Prepend & Replace should be combined");
                           end;
                        end;
                        --  Replace First, Prepend Second
                        declare
                           TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
                        begin
                           TR.Replace (Unit.Root, Replace);
                           TR.Prepend (Unit.Root, Prepend);
                           declare
                              Result       : constant String := TR.ApplyToString;
                           begin
                              Assert (Result = Prepend & Replace,
                                      "Prepend & Replace should be combined");
                           end;
                        end;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Test_Prepend_Replace;

   procedure Test_Prepend_Replace_Replace
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Unit        : constant Analysis_Unit := Analyze_Fragment (" f ( x ) ", Expr_Rule);
   begin
      Assert (Condition => Unit.Root.Kind = Ada_Call_Expr,
              Message => "Unexpectedly not a Call Expr, but " & Unit.Root.Kind'Image);
      declare
         C_E : constant Call_Expr := Unit.Root.As_Call_Expr;
      begin
         Assert (Condition => C_E.F_Suffix.Kind = Ada_Assoc_List,
                 Message => "Unexpectedly not an Assoc_List, but " & C_E.F_Suffix.Kind'Image);
         declare
            A_L : constant Assoc_List := C_E.F_Suffix.As_Assoc_List;
         begin
            Assert (Condition => A_L.Children_Count = 1,
                    Message => "Unexpectedly not an singleton Assoc_List, but "
                    & A_L.Children_Count'Image);
            declare
               Arg : constant Ada_Node := A_L.First_Child;
               TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
            begin
               TR.Prepend (C_E, "p.");
               TR.Replace (Arg, "y");
               TR.Replace (C_E, "g (12)");
               declare
                  Result       : constant String := TR.ApplyToString;
               begin
                  Assert (Expected => " p.g (12) ",
                          Actual   => Result,
                          Message  => "First replace is overwritten");
               end;
            end;
         end;
      end;
   end Test_Prepend_Replace_Replace;

   procedure Test_Replace_Append
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      for UExpr_String of Identifiers loop
         declare
            Expr_String : constant String := To_String (UExpr_String);
            Unit        : constant Analysis_Unit := Analyze_Fragment (Expr_String, Expr_Rule);
         begin
            for UAppend of Strings loop
               declare
                  Append      : constant String := To_String (UAppend);
               begin
                  for UReplace of Strings loop
                     declare
                        Replace      : constant String := To_String (UReplace);
                     begin
                        --  Append First, Replace Second
                        declare
                           TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
                        begin
                           TR.Append (Unit.Root, Append);
                           TR.Replace (Unit.Root, Replace);
                           declare
                              Result       : constant String := TR.ApplyToString;
                           begin
                              Assert (Result = Replace & Append,
                                      "Replace & Append should be combined");
                           end;
                        end;
                        --  Replace First, Append Second
                        declare
                           TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
                        begin
                           TR.Replace (Unit.Root, Replace);
                           TR.Append (Unit.Root, Append);
                           declare
                              Result       : constant String := TR.ApplyToString;
                           begin
                              Assert (Result = Replace & Append,
                                      "Replace & Append should be combined");
                           end;
                        end;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Test_Replace_Append;

   procedure Test_Append_Replace_Replace
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Unit        : constant Analysis_Unit := Analyze_Fragment (" f ( x ) ", Expr_Rule);
   begin
      Assert (Condition => Unit.Root.Kind = Ada_Call_Expr,
              Message => "Unexpectedly not a Call Expr, but " & Unit.Root.Kind'Image);
      declare
         C_E : constant Call_Expr := Unit.Root.As_Call_Expr;
      begin
         Assert (Condition => C_E.F_Suffix.Kind = Ada_Assoc_List,
                 Message => "Unexpectedly not an Assoc_List, but " & C_E.F_Suffix.Kind'Image);
         declare
            A_L : constant Assoc_List := C_E.F_Suffix.As_Assoc_List;
         begin
            Assert (Condition => A_L.Children_Count = 1,
                    Message => "Unexpectedly not an singleton Assoc_List, but "
                    & A_L.Children_Count'Image);
            declare
               Arg : constant Ada_Node := A_L.First_Child;
               TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
            begin
               TR.Append (C_E, ".z");
               TR.Replace (Arg, "y");
               TR.Replace (C_E, "g (12)");
               declare
                  Result       : constant String := TR.ApplyToString;
               begin
                  Assert (Expected => " g (12).z ",
                          Actual   => Result,
                          Message  => "First replace is overwritten");
               end;
            end;
         end;
      end;
   end Test_Append_Replace_Replace;

   procedure Test_Adjacent_Append_Prepend
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Stmts_String : constant String       := "x:=y+z;k:=l-m;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmts_String, Stmts_Rule);
      Append      : constant String        := "a";
      Prepend     : constant String        := "p";

      procedure Test_Adjacent_Append_Prepend (Node : Ada_Node)
      is
      begin
         declare
            Next : constant Ada_Node := Node.Next_Sibling;
         begin
            if not Next.Is_Null then
               declare
                  TR          :          Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
               begin
                  --  TODO: also make the other order (Prepend & Append) correct
                  TR.Append (Node, Append);
                  TR.Prepend (Next, Prepend);
                  declare
                     Result        : constant String  := TR.ApplyToString;
                     Index_Append  : constant Natural :=
                       Index (Source => Result, Pattern => Append);
                     Index_Prepend : constant Natural :=
                       Index (Source => Result, Pattern => Prepend);
                  begin
                     Assert (Index_Append + Append'Length <= Index_Prepend,
                             "Append of node should be before prepend of next sibling");
                  end;
               end;
            end if;
         end;
      end Test_Adjacent_Append_Prepend;

      Nodes : constant Node_List.Vector := Find (Unit.Root, Is_A_Node'Access);
   begin
      Assert_Unique_Nodes (Nodes);
      Assert_Unique_Insert (Nodes, Append);
      Assert_Unique_Insert (Nodes, Prepend);
      Assert_Unique_Strings (Append, Prepend);

      for Node of Nodes loop
         Test_Adjacent_Append_Prepend (Node);
      end loop;
   end Test_Adjacent_Append_Prepend;

   procedure Test_Replace_Idempotent
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);

      Stmt_String : constant String        := "a := b + c;";
      Unit        : constant Analysis_Unit := Analyze_Fragment (Stmt_String, Stmt_Rule);
      Replacement : constant String        := "anything goes";

      procedure Test_Replace_Idempotent (Node : Ada_Node)
      is
         TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
      begin
         TR.Replace (Node, Replacement);
         declare
            Result_Once : constant String := TR.ApplyToString;
         begin
            Assert (Result_Once /= Stmt_String, "Result should be different from orginal text");

            TR.Replace (Node, Replacement);
            declare
               Result_Twice : constant String := TR.ApplyToString;
            begin
               Assert (Result_Twice, Result_Once,
                       "Replace should be idempotent, yet actual and expected differ.");
            end;
         end;
      end Test_Replace_Idempotent;

      Nodes : constant Node_List.Vector := Find (Unit.Root, Is_A_Node'Access);
   begin
      Assert_Unique_Insert (Nodes, Replacement);

      for Node of Nodes loop
         Test_Replace_Idempotent (Node);
      end loop;
   end Test_Replace_Idempotent;

   procedure Test_Node_Location_Before (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Comment_Line_Before : constant String := "-- comment line before" & ASCII.CR & ASCII.LF;
      Indentation_Before : constant String := "   ";

      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Comment_Line_Before
                          & Indentation_Before
                          & Make_Procedure_Call_Statement,
                          Call_Stmt_Rule);

      package Map_Node_Location_Expected_Type is
        new Ada.Containers.Ordered_Maps (Key_Type => Node_Location,
                                         Element_Type => Unbounded_String);
      use Map_Node_Location_Expected_Type;

      Expectation_Map : Map_Node_Location_Expected_Type.Map;
   begin
      Expectation_Map.Insert (No_Trivia,
                              To_Unbounded_String (Comment_Line_Before & Indentation_Before));
      Expectation_Map.Insert (Trivia_On_Same_Line,
                              To_Unbounded_String (Comment_Line_Before));
      Expectation_Map.Insert (All_Trivia,
                              To_Unbounded_String (""));

      for C in Expectation_Map.Iterate loop
         declare
            TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Instance);
         begin
            TR.Remove (Instance.Root, Before => Key (C));
            declare
               Result : constant String := TR.ApplyToString;
            begin
               Assert (Expected => To_String (Element (C)),
                       Actual   => Result,
                       Message  => "Unexpected result for " & Key (C)'Image);
            end;
         end;
      end loop;
   end Test_Node_Location_Before;

   procedure Test_Node_Location_Before_No_Newline (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Indentation_Before : constant String := "   ";

      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Indentation_Before & Make_Procedure_Call_Statement,
                          Call_Stmt_Rule);

      package Map_Node_Location_Expected_Type is
        new Ada.Containers.Ordered_Maps (Key_Type => Node_Location,
                                         Element_Type => Unbounded_String);
      use Map_Node_Location_Expected_Type;

      Expectation_Map : Map_Node_Location_Expected_Type.Map;
   begin
      Expectation_Map.Insert (No_Trivia,
                              To_Unbounded_String (Indentation_Before));
      Expectation_Map.Insert (Trivia_On_Same_Line,
                              To_Unbounded_String (""));
      Expectation_Map.Insert (All_Trivia,
                              To_Unbounded_String (""));

      for C in Expectation_Map.Iterate loop
         declare
            TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Instance);
         begin
            TR.Remove (Instance.Root, Before => Key (C));
            declare
               Result : constant String := TR.ApplyToString;
            begin
               Assert (Expected => To_String (Element (C)),
                       Actual   => Result,
                       Message  => "Unexpected result for " & Key (C)'Image);
            end;
         end;
      end loop;
   end Test_Node_Location_Before_No_Newline;

   procedure Test_Node_Location_After (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Indentation_After : constant String := "     ";
      Comment_After : constant String := "-- comment" & ASCII.CR & ASCII.LF;

      Comment_Line_After : constant String := "-- comment line after" & ASCII.CR & ASCII.LF;

      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Make_Procedure_Call_Statement
                          & Indentation_After
                          & Comment_After
                          & Comment_Line_After,
                          Call_Stmt_Rule);

      package Map_Node_Location_Expected_Type is
        new Ada.Containers.Ordered_Maps (Key_Type => Node_Location,
                                         Element_Type => Unbounded_String);
      use Map_Node_Location_Expected_Type;

      Expectation_Map : Map_Node_Location_Expected_Type.Map;
   begin
      Expectation_Map.Insert (No_Trivia,
                              To_Unbounded_String (
                                Indentation_After
                                & Comment_After
                                & Comment_Line_After));
      Expectation_Map.Insert (Trivia_On_Same_Line,
                              To_Unbounded_String (Comment_Line_After));
      Expectation_Map.Insert (All_Trivia,
                              To_Unbounded_String (""));

      for C in Expectation_Map.Iterate loop
         declare
            TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Instance);
         begin
            TR.Remove (Instance.Root, After => Key (C));
            declare
               Result : constant String := TR.ApplyToString;
            begin
               Assert (Expected => To_String (Element (C)),
                       Actual   => Result,
                       Message  => "Unexpected result for " & Key (C)'Image);
            end;
         end;
      end loop;
   end Test_Node_Location_After;

   procedure Test_Node_Location_After_No_Newline (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Indentation_After : constant String := "     ";
      Comment_After : constant String := "-- comment";

      Instance : constant Analysis_Unit :=
        Analyze_Fragment (Make_Procedure_Call_Statement
                          & Indentation_After
                          & Comment_After,
                          Call_Stmt_Rule);

      package Map_Node_Location_Expected_Type is
        new Ada.Containers.Ordered_Maps (Key_Type => Node_Location,
                                         Element_Type => Unbounded_String);
      use Map_Node_Location_Expected_Type;

      Expectation_Map : Map_Node_Location_Expected_Type.Map;
   begin
      Expectation_Map.Insert (No_Trivia,
                              To_Unbounded_String (
                                Indentation_After
                                & Comment_After));
      Expectation_Map.Insert (Trivia_On_Same_Line,
                              To_Unbounded_String (""));
      Expectation_Map.Insert (All_Trivia,
                              To_Unbounded_String (""));

      for C in Expectation_Map.Iterate loop
         declare
            TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Instance);
         begin
            TR.Remove (Instance.Root, After => Key (C));
            declare
               Result : constant String := TR.ApplyToString;
            begin
               Assert (Expected => To_String (Element (C)),
                       Actual   => Result,
                       Message  => "Unexpected result for " & Key (C)'Image);
            end;
         end;
      end loop;
   end Test_Node_Location_After_No_Newline;

   --  Test plumbing

   overriding function Name
     (T : Text_Rewrite_Test_Case)
      return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Text Rewrite");
   end Name;

   overriding procedure Register_Tests
     (T : in out Text_Rewrite_Test_Case)
   is
   begin
      Registration.Register_Routine (T, Test_None'Access, "None");
      Registration.Register_Routine (T, Test_Replace_Root'Access, "Replace Root");
      Registration.Register_Routine (T, Test_Prepend'Access, "Prepend");
      Registration.Register_Routine (T, Test_Append'Access, "Append");
      Registration.Register_Routine (T,
                                     Test_Prepend_Replace'Access,
                                     "Prepend Replace interaction");
      Registration.Register_Routine (T,
                                     Test_Prepend_Replace_Replace'Access,
                                     "Prepend Replace Replace interaction");
      Registration.Register_Routine (T, Test_Replace_Append'Access,
                                     "Replace Append interaction");
      Registration.Register_Routine (T,
                                     Test_Append_Replace_Replace'Access,
                                     "Append Replace Replace interaction");
      Registration.Register_Routine (T,
                                     Test_Adjacent_Append_Prepend'Access,
                                     "Append before adjacent prepend");
      Registration.Register_Routine (T, Test_Replace_Idempotent'Access,
                                     "Replace is idempotent");
      Registration.Register_Routine (T,
                                     Test_Node_Location_Before'Access,
                                     "Node location - Before");
      Registration.Register_Routine (T,
                                     Test_Node_Location_Before_No_Newline'Access,
                                     "Node location - Before (no newline)");
      Registration.Register_Routine (T, Test_Node_Location_After'Access,
                                     "Node location - After");
      Registration.Register_Routine (T,
                                     Test_Node_Location_After_No_Newline'Access,
                                     "Node location - After (no newline)");
   end Register_Tests;

end Test_Text_Rewrites;
