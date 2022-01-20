with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Rejuvenation.Factory;
with Rejuvenation.Finder;
with Rejuvenation.Navigation;
with Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Basic_Declaration;
with Basic_Subprogram_Calls;
with Generic_Subprogram_Calls;
with Task_Subprogram_Calls;
with Aspect_Subprogram_Calls;
with Tagged_Subprogram_Calls;
with Operator_Subprogram_Calls;
with Test_Call_Filtering;
with Test_Operator_Attribute;
with Package_Without_Body;
with Subprogram_Unit;
with Subprogram_Unit_2;
with Deferred_Constant;
with Forward_Declaration;

procedure Main is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Context : Rejuvenation.Factory.Project_Context := Rejuvenation.Factory.Open_Project("syntax_examples.gpr");
   Units : Rejuvenation.Factory.Analysis_Unit_Vectors.Vector := Rejuvenation.Factory.Open_Files_From_Project(Context, False);

   function Get_Filename(Node : LAL.Ada_Node'Class) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Unit_Filename : constant String := Node.Unit.Get_Filename;
      Filename_Start : constant Natural := Index(Unit_Filename, "\", Backward);
   begin
      return Delete(Unit_Filename, Unit_Filename'First, Filename_Start);
   end Get_Filename;

   function Get_Location(Node : LAL.Ada_Node'Class) return String is
      Filename : constant String := Get_Filename(Node);
      Sloc_Range : constant String := Langkit_Support.Slocs.Image(Node.Sloc_Range);
   begin
      return Filename & "[" & Sloc_Range & "]";
   end Get_Location;

   function Find_Node_Of_Kind(Node : LAL.Ada_Node'Class; Node_Kind : LALCO.Ada_Node_Kind_Type) return Boolean is
      Filename : constant String := Get_Filename(Node);
      Found : Boolean := False;
   begin
      for Node_Of_Kind of Rejuvenation.Finder.Find(Node, Node_Kind) loop
         Found := True;
         Ada.Text_IO.New_Line;
         Node_Of_Kind.Print(Line_Prefix => Filename & ": ");
         Ada.Text_IO.New_Line;
      end loop;

      return Found;
   end Find_Node_Of_Kind;

   procedure Find_Basic_Decls is
      use type LALCO.Ada_Basic_Decl;
      package Ada_Basic_Decl_Vectors is new Ada.Containers.Vectors(Positive, LALCO.Ada_Basic_Decl);
      use Ada_Basic_Decl_Vectors;

      Unit_Specification, Unit_Body : LAL.Analysis_Unit;
      Missing_Basic_Decls : Vector;
   begin
      Unit_Specification := Rejuvenation.Factory.Open_File("src/basic_declaration.ads", Context);
      Unit_Body := Rejuvenation.Factory.Open_File("src/basic_declaration.adb", Context);

      for Node_Kind in LALCO.Ada_Basic_Decl loop
         if Node_Kind not in LALCO.Synthetic_Nodes then
            Ada.Text_IO.Put_Line("=== " & Node_Kind'Image & " ===");

            if not Find_Node_Of_Kind(Unit_Specification.Root, Node_Kind)
              and not Find_Node_Of_Kind(Unit_Body.Root, Node_Kind) then
               Missing_Basic_Decls.Append(Node_Kind);
            end if;
         end if;
      end loop;

      if Missing_Basic_Decls /= Empty_Vector then
         Ada.Text_IO.Put_Line("Missing basic declarations:");
         for Node_Kind of Missing_Basic_Decls loop
            Ada.Text_IO.Put_Line(" - " & Node_Kind'Image);
         end loop;
      end if;
   end Find_Basic_Decls;

   procedure Find_Calls is
      Unit : LAL.Analysis_Unit;

      function Visit(Node : LAL.Ada_Node'Class) return LALCO.Visit_Status is
         use LAL;
         use LALCO;
         use Langkit_Support.Text;
      begin
         --if Node.Kind = Ada_Identifier
         --  and then Node.Parent.Kind = Ada_Attribute_Ref
         --  and then Node.Parent.As_Attribute_Ref.F_Attribute = Node
         --then
         --   Node.Parent.Parent.Print;
         --   Ada.Text_IO.Put_Line(Node.As_Identifier.P_Is_Defining'Image);
         --   Ada.Text_IO.Put_Line(Node.As_Identifier.P_Referenced_Decl.Is_Null'Image);
         --   Ada.Text_IO.Put_Line(Node.As_Identifier.P_Is_Call'Image);
         --   Ada.Text_IO.Put_Line(Node.Parent.As_Attribute_Ref.P_Referenced_Decl.Is_Null'Image);
         --end if;

         if Node.Kind = Ada_Identifier
           and then not Node.As_Identifier.P_Is_Defining
           and then Node.As_Identifier.P_Is_Call
         then
            declare
               Identifier : LAL.Identifier := Node.As_Identifier;
               Identifier_Parent : LAL.Basic_Decl := Identifier.P_Parent_Basic_Decl;
               Identifier_Parent_Name : String :=
                 Encode(Identifier_Parent.P_Fully_Qualified_Name, Identifier_Parent.Unit.Get_Charset) & " at " & Get_Location(Identifier);
               Basic_Decl : LAL.Basic_Decl := Identifier.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Basic_Decl.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("1) " & Identifier_Parent_Name & " calls " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Identifier_Parent_Name & " is " & Identifier.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);

               -- below should be
               -- - look at parent, and if dotted name look at type of prefix
               -- - look at parent (or parent of parent), and if call expr look at types of parameters

               --if Call_Expr /= No_Ada_Node
               --  and then (for some E of Call_Expr.As_Call_Expr.F_Suffix.As_Assoc_List => E.As_Param_Assoc.F_R_Expr.P_Expression_Type.Kind = Ada_Classwide_Type_Decl)
               --then
               --   Ada.Text_IO.Put_Line("   - is dispatching");
               --end if;

               --if Identifier.P_Is_Dot_Call
               --  and then not Dotted_Name.As_Dotted_Name.F_Prefix.P_Expression_Type.Is_Null
               --then
               --   if Dotted_Name.As_Dotted_Name.F_Prefix.P_Expression_Type.Kind = Ada_Classwide_Type_Decl then -- Crashes in print if this check is missing
               --      Dotted_Name.As_Dotted_Name.F_Prefix.Print;
               --      Dotted_Name.As_Dotted_Name.F_Prefix.P_Expression_Type.Print;

               --      if Call_Expr /= No_Ada_Node
               --        and then not Call_Expr.As_Call_Expr.P_Expression_Type.Is_Null
               --      then
               --         Call_Expr.As_Call_Expr.P_Expression_Type.Print;
               --      end if;
               --   end if;

               --end if;

            end;
         elsif Node.Kind = Ada_Identifier
           and then Node.Parent.Kind /= Ada_End_Name
           and then not Node.As_Identifier.P_Is_Defining
           and then not Node.As_Identifier.P_Referenced_Decl.Is_Null
           and then Node.Parent.Kind not in Ada_Accept_Stmt_Range
           and then (Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Null_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Expr_Function
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Entry_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Entry_Body
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Subp_Body
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Subp_Renaming_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Generic_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Generic_Subp_Renaming_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Generic_Subp_Instantiation
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Subp_Body_Stub
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Abstract_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Abstract_Formal_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Concrete_Formal_Subp_Decl
                     or else Node.As_Identifier.P_Referenced_Decl.Kind = Ada_Enum_Literal_Decl) -- Enum literals are parameterless functions
           and then (Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Generic_Package_Instantiation) = No_Ada_Node -- Skip designator in instantiation
                     or else Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Param_Assoc) = No_Ada_Node
                     or else Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Param_Assoc).As_Param_Assoc.F_Designator /= Node)
           and then (Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Generic_Subp_Instantiation) = No_Ada_Node  -- Skip designator in instantiation
                     or else Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Param_Assoc) = No_Ada_Node
                     or else Rejuvenation.Navigation.Get_Ancestor_Of_Type(Node, Ada_Param_Assoc).As_Param_Assoc.F_Designator /= Node)
         then
            declare
               Identifier : LAL.Identifier := Node.As_Identifier;
               Identifier_Parent : LAL.Basic_Decl:= Identifier.P_Parent_Basic_Decl;
               Identifier_Parent_Name : String :=
                 Encode(Identifier_Parent.P_Fully_Qualified_Name, Identifier.Unit.Get_Charset) & " at " & Get_Location(Identifier);
               Basic_Decl : LAL.Basic_Decl := Identifier.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Node.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("2) " & Identifier_Parent_Name & " references " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Identifier_Parent_Name & " is " & Identifier.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Subp_Decl
         then
            declare
               Subp_Decl : LAL.Subp_Decl := Node.As_Subp_Decl;
               Subp_Decl_Name : String :=
                 Encode(Subp_Decl.P_Fully_Qualified_Name, Subp_Decl.Unit.Get_Charset) & " at " & Get_Location(Subp_Decl);
               Subp_Body : LAL.Body_Node := Subp_Decl.P_Body_Part_For_Decl;
               Subp_Body_Name : String :=
                 Encode(Subp_Body.P_Fully_Qualified_Name, Subp_Body.Unit.Get_Charset) & " at " & Get_Location(Subp_Body);
               -- Foo : Basic_Decl_Array := Subp_Decl.P_Base_Subp_Declarations; -- Finds subprograms overriden (including the current one)
            begin
               Ada.Text_IO.Put_Line("3) " & Subp_Decl_Name & " is implemented by " & Subp_Body_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Decl_Name & " is " & Subp_Decl.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Body_Name & " is " & Subp_Body.Unit.Get_Filename);

               -- for I in Foo'Range loop
               --   Foo(I).Print;
               --end loop;
            end;
         elsif Node.Kind = Ada_Generic_Subp_Decl
         then
            declare
               Subp_Decl : LAL.Generic_Subp_Decl := Node.As_Generic_Subp_Decl;
               Subp_Decl_Name : String :=
                 Encode(Subp_Decl.P_Fully_Qualified_Name, Subp_Decl.Unit.Get_Charset) & " at " & Get_Location(Subp_Decl);
               Subp_Body : LAL.Body_Node := Subp_Decl.P_Body_Part_For_Decl;
               Subp_Body_Name : String :=
                 Encode(Subp_Body.P_Fully_Qualified_Name, Subp_Body.Unit.Get_Charset) & " at " & Get_Location(Subp_Body);
            begin
               Ada.Text_IO.Put_Line("4) " & Subp_Decl_Name & " is implemented by " & Subp_Body_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Decl_Name & " is " & Subp_Decl.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Body_Name & " is " & Subp_Body.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Subp_Body_Stub then
            declare
               Subp_Body_Stub : LAL.Subp_Body_Stub := Node.As_Subp_Body_Stub;
               Subp_Body_Stub_Name : String :=
                 Encode(Subp_Body_Stub.P_Fully_Qualified_Name, Subp_Body_Stub.Unit.Get_Charset) & " at " & Get_Location(Subp_Body_Stub);
               Subp_Body : LAL.Body_Node := Subp_Body_Stub.P_Body_Part_For_Decl;
               Subp_Body_Name : String :=
                 Encode(Subp_Body.P_Fully_Qualified_Name, Subp_Body.Unit.Get_Charset) & " at " & Get_Location(Subp_Body);
            begin
               Ada.Text_IO.Put_Line("5) " & Subp_Body_Stub_Name & " is implemented by " & Subp_Body_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Body_Stub_Name & " is " & Subp_Body_Stub.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Body_Name & " is " & Subp_Body.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Subp_Renaming_Decl
           and then not Node.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object.P_Referenced_Decl.Is_Null
         then
            declare
               Subp_Renaming_Decl : LAL.Subp_Renaming_Decl := Node.As_Subp_Renaming_Decl;
               Subp_Renaming_Decl_Name : String :=
                 Encode(Subp_Renaming_Decl.P_Fully_Qualified_Name, Subp_Renaming_Decl.Unit.Get_Charset) & " at " & Get_Location(Subp_Renaming_Decl);
               Basic_Decl : LAL.Basic_Decl := Subp_Renaming_Decl.F_Renames.F_Renamed_Object.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Node.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("6) " & Subp_Renaming_Decl_Name & " renames " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Renaming_Decl_Name & " is " & Subp_Renaming_Decl.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Generic_Subp_Renaming_Decl then
            declare
               Subp_Renaming_Decl : LAL.Generic_Subp_Renaming_Decl := Node.As_Generic_Subp_Renaming_Decl;
               Subp_Renaming_Decl_Name : String :=
                 Encode(Subp_Renaming_Decl.P_Fully_Qualified_Name, Subp_Renaming_Decl.Unit.Get_Charset) & " at " & Get_Location(Subp_Renaming_Decl);
               Basic_Decl : LAL.Basic_Decl := Subp_Renaming_Decl.F_Renames.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Node.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("7) " & Subp_Renaming_Decl_Name & " renames " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Renaming_Decl_Name & " is " & Subp_Renaming_Decl.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Generic_Subp_Instantiation then
            declare
               Subp_Instantiation : LAL.Generic_Subp_Instantiation := Node.As_Generic_Subp_Instantiation;
               Subp_Instantiation_Name : String :=
                 Encode(Subp_Instantiation.P_Fully_Qualified_Name, Subp_Instantiation.Unit.Get_Charset) & " at " & Get_Location(Subp_Instantiation);
               Basic_Decl : LAL.Basic_Decl := Subp_Instantiation.F_Generic_Subp_Name.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Node.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("8) " & Subp_Instantiation_Name & " instantiates " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Subp_Instantiation_Name & " is " & Subp_Instantiation.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Entry_Body then -- We do not reason starting from the entry declaration, to be consistent with accept statements
            declare
               Entry_Body : LAL.Entry_Body := Node.As_Entry_Body;
               Entry_Body_Name : String :=
                 Encode(Entry_Body.P_Fully_Qualified_Name, Entry_Body.Unit.Get_Charset) & " at " & Get_Location(Entry_Body);
               Entry_Decl : LAL.Basic_Decl := Entry_Body.P_Decl_Part;
               Entry_Decl_Name : String :=
                 Encode(Entry_Decl.P_Fully_Qualified_Name, Entry_Decl.Unit.Get_Charset) & " at " & Get_Location(Entry_Decl);
            begin
               Ada.Text_IO.Put_Line("9) " & Entry_Body_Name & " implements " & Entry_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Entry_Body_Name & " is " & Entry_Body.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Entry_Decl_Name & " is " & Entry_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Accept_Stmt then -- We do not reason starting from the entry declaration, because there can be multiple accepts for the same entry.
            declare
               Accept_Stmt : LAL.Accept_Stmt := Node.As_Accept_Stmt;
               Accept_Stmt_Line : String := Accept_Stmt.Sloc_Range.Start_Line'Image;
               Accept_Stmt_Parent : LAL.Basic_Decl := Accept_Stmt.P_Parent_Basic_Decl;
               Accept_Stmt_Parent_Name : String :=
                 Encode(Accept_Stmt_Parent.P_Fully_Qualified_Name, Accept_Stmt.Unit.Get_Charset) & " at " & Get_Location(Accept_Stmt);
               Entry_Decl : LAL.Entry_Decl := Accept_Stmt.F_Name.P_Referenced_Decl.As_Entry_Decl;
               Entry_Decl_Name : String :=
                 Encode(Entry_Decl.P_Fully_Qualified_Name, Entry_Decl.Unit.Get_Charset) & " at " & Get_Location(Entry_Decl);
            begin
               Ada.Text_IO.Put_Line("10) " & Accept_Stmt_Parent_Name & " accepts " & Entry_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Accept_Stmt_Parent_Name & " is " & Accept_Stmt.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Entry_Decl_Name & " is " & Entry_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind = Ada_Accept_Stmt_With_Stmts then  -- We do not reason starting from the entry declaration, because there can be multiple accepts for the same entry.
            declare
               Accept_Stmt : LAL.Accept_Stmt_With_Stmts := Node.As_Accept_Stmt_With_Stmts;
               Accept_Stmt_Line : String := Accept_Stmt.Sloc_Range.Start_Line'Image;
               Accept_Stmt_Parent : LAL.Basic_Decl := Accept_Stmt.P_Parent_Basic_Decl;
               Accept_Stmt_Parent_Name : String :=
                 Encode(Accept_Stmt_Parent.P_Fully_Qualified_Name, Accept_Stmt.Unit.Get_Charset) & " at " & Get_Location(Accept_Stmt);
               Entry_Decl : LAL.Entry_Decl := Accept_Stmt.F_Name.P_Referenced_Decl.As_Entry_Decl;
               Entry_Decl_Name : String :=
                 Encode(Entry_Decl.P_Fully_Qualified_Name, Entry_Decl.Unit.Get_Charset) & " at " & Get_Location(Entry_Decl);
            begin
               Ada.Text_IO.Put_Line("11) " & Accept_Stmt_Parent_Name & " accepts " & Entry_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Accept_Stmt_Parent_Name & " is " & Accept_Stmt.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Entry_Decl_Name & " is " & Entry_Decl.Unit.Get_Filename);
            end;
         elsif Node.Kind in Ada_Op
           and then not Node.As_Op.P_Referenced_Decl.Is_Null
         then
            declare
               Op : LAL.Op := Node.As_Op;
               Op_Line : String := Op.Sloc_Range.Start_Line'Image;
               Op_Parent : LAL.Basic_Decl := Op.P_Parent_Basic_Decl;
               Op_Parent_Name : String :=
                 Encode(Op_Parent.P_Fully_Qualified_Name, Op.Unit.Get_Charset) & " at " & Get_Location(Op);
               Basic_Decl : LAL.Basic_Decl := Op.P_Referenced_Decl;
               Basic_Decl_Name : String :=
                 Encode(Basic_Decl.P_Fully_Qualified_Name, Node.Unit.Get_Charset) & " at " & Get_Location(Basic_Decl);
            begin
               Ada.Text_IO.Put_Line("12) " & Op_Parent_Name & " calls " & Basic_Decl_Name);
               Ada.Text_IO.Put_Line("   - full path of " & Op_Parent_Name & " is " & Op.Unit.Get_Filename);
               Ada.Text_IO.Put_Line("   - full path of " & Basic_Decl_Name & " is " & Basic_Decl.Unit.Get_Filename);
            end;
         end if;

         return Into;
      exception when E : Property_Error =>
            Ada.Text_IO.Put_Line("Encountered Libadalang problem: " & Ada.Exceptions.Exception_Message (E));
            Node.Print;
            return Into;
      end Visit;

   begin
      for Unit of Units loop
         Ada.Text_IO.Put_Line("== " & Unit.Get_Filename & " ==");
         Unit.Root.Traverse(Visit'Access);
      end loop;
   end Find_Calls;

begin
   -- Find_Basic_Decls;
   Find_Calls;
end Main;
