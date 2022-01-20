package body Basic_Declaration is

   task body Type_Declaration_Name_5 is
      Cons_3 : constant := 42;
   begin
      null;
   end Type_Declaration_Name_5;
   
   protected body Type_Declaration_Name_6 is
      entry Entry_Declaration_Name(for Entry_Index_Specification_Name in Boolean) when True is
         Cons_4 : constant := 42;
      begin
         null;
      end Entry_Declaration_Name;
   end Type_Declaration_Name_6;
   
   task body Object_Declaration_Name_2 is
   begin
      null;
   end Object_Declaration_Name_2;

   protected body Object_Declaration_Name_3 is
   end Object_Declaration_Name_3;

   procedure Subprogram_Declaration_Name_1(Parameter_Name : Type_Declaration_Name_3) is
      Local_Object_Declaration_Name_1 : Type_Declaration_Name_3'Class := Parameter_Name;
      Local_Object_Declaration_Name_2 : Type_Declaration_Name_4 := Local_Object_Declaration_Name_1.Subprogram_Declaration_Name_2;
      Local_Object_Declaration_Name_3 : Integer;
   begin
      
      Local_Object_Declaration_Name_3 := Local_Object_Declaration_Name_2.Component_Name_2;
      
      for Defining_Identifier_Name in 1 .. Local_Object_Declaration_Name_3 loop
         null;
      end loop;
      
      Statement_Identifier_Name:
      begin
         null;
      exception
         when Choice_Parameter_Specification_Name: Exception_Declaration_Name =>
            raise;
      end Statement_Identifier_Name;
      
      <<Label_Statement_Identifier_Name>> null;
   end Subprogram_Declaration_Name_1;

   function Subprogram_Declaration_Name_2(Parameter_Name : Type_Declaration_Name_3'Class) return Type_Declaration_Name_4 is
   begin
      return Extended_Return_Object_declaration_Name : Type_Declaration_Name_4(True) do
         Extended_Return_Object_declaration_Name.Component_Name_2 := 42;
      end return;
   end Subprogram_Declaration_Name_2;

   procedure Generic_Declaration_Name_2 is
      Cons_2 : constant := 42;
   begin
      null;
   end;

   procedure Generic_Instantiation_Name_2 is new Generic_Declaration_Name_2(Generic_Instantiation_Name_1);

   procedure Subprogram_Body_Stub_Name is separate;
   
   package body Package_Body_Stub_Name is separate;
   
   task body Task_Body_Stub_Name is separate;
   
   protected body Protected_Body_Stub_Name is separate;

begin
   Object_Declaration_Name_1 := 42;
end Basic_Declaration;
