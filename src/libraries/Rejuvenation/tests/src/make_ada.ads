with String_Vectors;       use String_Vectors;
with String_Vectors_Utils; use String_Vectors_Utils;
with Ada.Containers;       use Ada.Containers;
--  TODO:
--  asked Ada expert how to get operators of String_Vectors
--  without also including Ada.Containers
--  See https://gt3-prod-1.adacore.com/#/tickets/UC02-053 for answer...

--  All parameters of the (externally visible)
--  Make-* functions have default values,
--  such that test cases can be easily vary one aspect of a production rule.
package Make_Ada is

   subtype ARM_Type_Definition is String;
   --  TODO: find good name that satisfy GNATCheck as well
   --  TODO: Make production rules type safe

   function Make_Subtype_Indication_Index_Constraints
     (Subtype_Mark      : String := "Integer";
      Index_Constraints : Vector := To_Vector ("1..10", 1)) return String;
   --  implements production rule
   --  subtype_indication ::=  [null_exclusion] subtype_mark [constraint]
   --  constraint ::= scalar_constraint | composite_constraint
   --  composite_constraint ::=
   --    index_constraint | discriminant_constraint
   --  See
   --  http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-2-2.html#S0027

   function Make_Object_Declaration_Subtype_Indication
     (Defining_Identifier_List : Vector := To_Vector ("Name", 1);
      Subtype_Indication : String := "Integer"; Expression : String := "";
      Aspect_List              : Vector := Empty_Vector) return String;
   --  implements production rule
   --
   --    defining_identifier_list :
   --    [aliased] [constant] subtype_indication
   --    [:= expression] [aspect_specification];
   --
   --  see
   --  http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-3-1.html#S0032
   --  Note: An absent expression is implemented
   --  using the empty string (i.e. "")

   function Make_Null_Statement return String;

   function Make_Parameter_Association
     (Name : String := ""; Value : String := "1") return String;

   function Make_Assignment_Statement
     (Variable_Name : String := "Name"; Expression : String := "1")
      return String;
   --  implements production rule
   --  assignment_statement ::= variable_name := expression;
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-5-2.html
   --  note LibAdaLang uses destination instead of variable name.

   function Make_Procedure_Call_Statement
     (Procedure_Name        : String := "P";
      Actual_Parameter_Part : Vector := Empty_Vector) return String;
   --  implements production rule
   --  procedure_call_statement ::= procedure_name;
   --                             | procedure_prefix actual_parameter_part;
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-6-4.html

   function Make_Function_Call
     (Function_Name         : String := "F";
      Actual_Parameter_Part : Vector := Empty_Vector) return String;
   --  implements production rule
   --  function_call ::= function_name | function_prefix actual_parameter_part
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-6-4.html

   function Make_Enumeration_Type_Definition
     (Defining_Identifier_List : Vector := "M" & "F")
      return ARM_Type_Definition with
      Pre => Defining_Identifier_List.Length > 0;
      --  implements production rule
      --  enumeration_type_definition ::=
--    (enumeration_literal_specification {, enumeration_literal_specification})
      --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-5-1.html

   function Make_Full_Type_Declaration
     (Defining_Identifier      : String              := "T";
      Type_Definition_Instance : ARM_Type_Definition :=
        Make_Enumeration_Type_Definition)
      return String;
   --  implements (part of) production rule
   --  full_type_declaration ::=
--      type defining_identifier [known_discriminant_part] is type_definition
   --      [aspect_specification];
   --    | task_type_declaration
   --    | protected_type_declaration
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-2-1.html

   function Make_If_Statement
     (Condition  : String := "C"; Stmts_True : String := Make_Null_Statement;
      Elsif_List : Vector := Empty_Vector; Stmts_False : String := "")
      return String;
   --  implements production rule
   --    if_statement ::=
   --         if condition then
   --           sequence_of_statements
   --        {elsif condition then
   --           sequence_of_statements}
   --        [else
   --           sequence_of_statements]
   --         end if;
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-5-3.html

   function Make_Handled_Sequence_Of_Statements
     (Sequence_Of_Statements : String := Make_Null_Statement;
      Exception_Handlers     : Vector := Empty_Vector) return String;
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-11-2.html

   function Make_Block_Statement
     (Declarative_Part               : String := "";
      Handled_Sequence_Of_Statements : String :=
        Make_Handled_Sequence_Of_Statements)
      return String;
   --  block_statement ::=
   --   [block_statement_identifier:]
   --       [declare
   --            declarative_part]
   --        begin
   --            handled_sequence_of_statements
   --        end [block_identifier];
   --  see http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-5-6.html

private

   function Make_Call
     (Name : String; Actual_Parameter_Part : Vector) return String;

   function Make_Subtype_Indication_Index_Constraints
     (Subtype_Mark      : String := "Integer";
      Index_Constraints : Vector := To_Vector ("1..10", 1)) return String is
     (Subtype_Mark & " (" & Join (Index_Constraints, ", ") & ")");

   function Make_Object_Declaration_Subtype_Indication
     (Defining_Identifier_List : Vector := To_Vector ("Name", 1);
      Subtype_Indication : String := "Integer"; Expression : String := "";
      Aspect_List              : Vector := Empty_Vector) return String is
     (Join (Defining_Identifier_List, ", ") & " : " & Subtype_Indication &
      (if Expression = "" then "" else " := " & Expression) &
      (if Aspect_List.Is_Empty then ""
       else " with " & Join (Aspect_List, ", ")) &
      ";");

   function Make_Null_Statement return String is ("null;");

   function Make_Assignment_Statement
     (Variable_Name : String := "Name"; Expression : String := "1")
      return String is
     (Variable_Name & " := " & Expression & ";");

   function Make_Procedure_Call_Statement
     (Procedure_Name        : String := "P";
      Actual_Parameter_Part : Vector := Empty_Vector) return String is
     (Make_Call (Procedure_Name, Actual_Parameter_Part) & ";");

   function Make_Function_Call
     (Function_Name         : String := "F";
      Actual_Parameter_Part : Vector := Empty_Vector) return String is
     (Make_Call (Function_Name, Actual_Parameter_Part));

   function Make_Enumeration_Type_Definition
     (Defining_Identifier_List : Vector := "M" & "F")
      return ARM_Type_Definition is
     ("(" & Join (Defining_Identifier_List, ", ") & ")");

   function Make_Full_Type_Declaration
     (Defining_Identifier      : String              := "T";
      Type_Definition_Instance : ARM_Type_Definition :=
        Make_Enumeration_Type_Definition)
      return String is
     ("type " & Defining_Identifier & " is " & Type_Definition_Instance & ";");

   function Make_If_Statement
     (Condition  : String := "C"; Stmts_True : String := Make_Null_Statement;
      Elsif_List : Vector := Empty_Vector; Stmts_False : String := "")
      return String is
     ("if " & Condition & " then " & Stmts_True & Join (Elsif_List, "") &
      (if Stmts_False = "" then "" else " else " & Stmts_False) & " end if;");

   function Make_Handled_Sequence_Of_Statements
     (Sequence_Of_Statements : String := Make_Null_Statement;
      Exception_Handlers     : Vector := Empty_Vector) return String is
     (Sequence_Of_Statements & Join (Exception_Handlers));

   function Make_Block_Statement
     (Declarative_Part               : String := "";
      Handled_Sequence_Of_Statements : String :=
        Make_Handled_Sequence_Of_Statements)
      return String is
     ((if Declarative_Part = "" then "" else "declare " & Declarative_Part) &
      " begin " & Handled_Sequence_Of_Statements & " end;");

end Make_Ada;
