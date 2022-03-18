package Basic_Declaration is

   -- basic_declaration ::= 
   --   type_declaration ::=
   --   | incomplete_type_declaration
   type Type_Declaration_Name_1;
   
   type Type_Declaration_Name_3 is tagged;
   --     full_type_declaration ::=
   --       type defining_identifier
   type Type_Declaration_Name_1 is range 1 .. 2;

   type Type_Declaration_Name_2 is
     (Enumeration_Literal_Specification_1, Enumeration_Literal_Specification_2);
   
   type Type_Declaration_Name_3 is tagged
      record
         Component_Name_1 : Integer;
      end record;

   type Type_Declaration_Name_4(Descriminant_Specification_Name : Boolean) is
      record
         Component_Name_2 : Integer;
      end record;
   --     | task_type_declaration
   task type Type_Declaration_Name_5;
   --     | protected_type_declaration
   protected type Type_Declaration_Name_6 is
      entry Entry_Declaration_Name(Boolean);
   end Type_Declaration_Name_6;
   --   | private_type_declaration
   type Type_Declaration_Name_7 is private;
   --   | private_extension_declaration
   type Type_Declaration_Name_8 is new Type_Declaration_Name_3 with private;

   -- | subtype_declaration
   subtype Subtype_Declaration_Name is Integer;

   -- | object_declaration ::=
   --     defining_indentifier_list : ...
   Object_Declaration_Name_1 : Integer;
   --   | single_task_declaration
   task Object_Declaration_Name_2;
   --   | single_protected_declaration
   protected Object_Declaration_Name_3 is
   end Object_Declaration_Name_3;
   
   -- | number_declaration
   Number_Declaration_Name : constant := 42;

   -- | subprogram_declaration ::=
   --     procedure_specification
   procedure Subprogram_Declaration_Name_1(Parameter_Name : Type_Declaration_Name_3);
   --   | function_specification
   function Subprogram_Declaration_Name_2(Parameter_Name : Type_Declaration_Name_3'Class) return Type_Declaration_Name_4;
   
   -- | abstract_subprogram_declaration ::=
   --     procedure_specification
   procedure Abstract_Subprogram_Declaration_Name_1 is abstract;
   --   | function_specification
   function Abstract_Subprogram_Declaration_Name_2 return Integer is abstract;
   
   -- | null_procedure_declaration
   procedure Null_Procedure_Declaration_Name is null;
     
   -- | expression_function_declaration
   function Expression_Function_Declaration_Name return Integer is
     (42);
   
   -- | package_declaration
   package Package_Declaration_Name is
   end Package_Declaration_Name;

   -- | exception_declaration
   Exception_Declaration_Name : exception;

   -- | generic_declaration
   generic
      Formal_Object_Declaration_Name : Integer := 42;
      type Formal_Type_Declaration_Name is private;
      with procedure Formal_Subprogram_Declaration_Name_1(Parameter_Name : Type_Declaration_Name_3);
      with procedure Formal_Subprogram_Declaration_Name_2(Parameter_Name : Type_Declaration_Name_3) is abstract;
   package Generic_Declaration_Name_1 is
   end Generic_Declaration_Name_1;

   generic
      with package Formal_Package_Declaration_Name is new Generic_Declaration_Name_1(<>);
   procedure Generic_Declaration_Name_2;
   -- | generic_instantiation
   package Generic_Instantiation_Name_1 is new Generic_Declaration_Name_1(
      Object_Declaration_Name_1, Integer, Subprogram_Declaration_Name_1, Subprogram_Declaration_Name_1);
   
   -- Generic instantiation of subprogram omitted here, as the body of the
   -- subprogram needs to be defined before the instantiation can be defined.

   -- | renaming_declaration ::=
   --     object_renaming_declaration
   Renaming_Declaration_Name_1 : Integer renames Object_Declaration_Name_1;
   --   | exception_renaming_declaration
   Renaming_Declaration_Name_2 : exception renames Exception_Declaration_Name;
   --   | package_renaming_declaration
   package Renaming_Declaration_Name_3 renames Package_Declaration_Name;
   --   | subprogram_renaming_declaration
   procedure Renaming_Declaration_Name_4 renames Null_Procedure_Declaration_Name;
   --   | generic_renaming_declaration
   generic package Renaming_Declaration_Name_5 renames Generic_Declaration_Name_1;
   
   generic procedure Renaming_Declaration_Name_6 renames Generic_Declaration_Name_2;
    
   -- body_stub ::=
   --   subprogram_body_stub
   procedure Subprogram_Body_Stub_Name;
   -- | package_body_stub
   package Package_Body_Stub_Name is
   end Package_Body_Stub_Name;
   -- | task_body_stub
   task type Task_Body_Stub_Name;
   -- | protected_body_stub
   protected type Protected_Body_Stub_Name is
   end Protected_Body_Stub_Name;
   
   -- Missing basic declarations:
   -- - ADA_DISCRETE_BASE_SUBTYPE_DECL
   -- - ADA_CLASSWIDE_TYPE_DECL:
   --     Occurs as designated type declaration when Type_Declaration_Name_3'Class
   --     is used, and refers to the type declaration of Type_Declaration_Name_3.
   -- - ADA_SYNTH_ANONYMOUS_TYPE_DECL
   -- - ADA_ERROR_DECL
   --     Occurs when a declaration did not parse correctly.
   
private
   
   type Type_Declaration_Name_7 is range 1 .. 2;

   type Type_Declaration_Name_8 is new Type_Declaration_Name_3 with
      record
         Component_Name_3 : access Integer;
      end record;

end Basic_Declaration;
pragma Elaborate_Body (Basic_Declaration);
