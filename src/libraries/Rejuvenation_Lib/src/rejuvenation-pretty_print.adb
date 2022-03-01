with Ada.Assertions;              use Ada.Assertions;
with Interfaces.C;                use Interfaces.C;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Indentation;    use Rejuvenation.Indentation;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;
with Rejuvenation.Nested;         use Rejuvenation.Nested;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Rejuvenation.String_Utils;   use Rejuvenation.String_Utils;

package body Rejuvenation.Pretty_Print is

   procedure Surround_Node_By_Pretty_Print_Section
     (T_R : in out Text_Rewrite'Class; Node : Ada_Node'Class)
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;

      function Predicate (Node : Ada_Node'Class) return Boolean
      --  workaround for https://gt3-prod-1.adacore.com/#/tickets/UB17-034
      --  not only look for node on separate lines,
      --  but also require a particular kind
      is
      begin
         return
           Node.Kind in Ada_Stmt | Ada_Stmt_List | Ada_Basic_Decl |
               Ada_Compilation_Unit
           and then Node_On_Separate_Lines (Node);
      end Predicate;

      Ctx : constant Ada_Node :=
        Get_Reflexive_Ancestor (Node, Predicate'Access);
   begin
      T_R.Prepend (Ctx, Pretty_Print_On, Before => Trivia_On_Same_Line);
      T_R.Append (Ctx, Pretty_Print_Off, After => Trivia_On_Same_Line);
   end Surround_Node_By_Pretty_Print_Section;

   procedure Turn_Pretty_Printing_Initially_Off
     (T_R : in out Text_Rewrite_Unit)
   is
      Unit : constant Analysis_Unit := T_R.Get_Unit;
   begin
      T_R.Prepend (Unit.Root, Pretty_Print_Off, All_Trivia, Unit.Get_Charset);
   end Turn_Pretty_Printing_Initially_Off;

   procedure Turn_Pretty_Printing_Initially_Off (Filename : String) is
      Original_Content : constant String := Get_String_From_File (Filename);
   begin
      Write_String_To_File (Pretty_Print_Off & Original_Content, Filename);
   end Turn_Pretty_Printing_Initially_Off;

   procedure Remove_Cr_Cr_Lf (Filename : String);
   procedure Remove_Cr_Cr_Lf (Filename : String)
     --  repair gnatpp screwed up
     --  see https://gt3-prod-1.adacore.com/#/tickets/U617-042
      is
      Contents       : constant String := Get_String_From_File (Filename);
      Final_Contents : constant String :=
        Replace_All
          (Contents, ASCII.CR & ASCII.CR & ASCII.LF, ASCII.CR & ASCII.LF);
   begin
      Write_String_To_File (Final_Contents, Filename);
   end Remove_Cr_Cr_Lf;

   procedure Remove_Nested_Pretty_Print_Flags (Filename : String);
   procedure Remove_Nested_Pretty_Print_Flags (Filename : String)
   is
      Contents       : constant String := Get_String_From_File (Filename);
      Final_Contents : constant String :=
        Remove_Nested_Flags (Contents, Pretty_Print_On, Pretty_Print_Off, 1);
   begin
      Write_String_To_File (Final_Contents, Filename);
   end Remove_Nested_Pretty_Print_Flags;

   procedure Pretty_Print_Sections (Filename : String; Projectname : String) is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");

      Command : constant String :=
        "gnatpp" & " -P " & Projectname & " --pp-on=" & Flag_On &
        " --pp-off=" & Flag_Off & " " & Filename;
      Ret_Val : Integer;
   begin
      Remove_Nested_Pretty_Print_Flags (Filename);
      declare
         Original_Content : constant String := Get_String_From_File (Filename);
         Original_Last_Char : constant Character :=
           Original_Content (Original_Content'Last);
      begin
         Ret_Val := Sys (To_C (Command));
         Assert
           (Check   => Ret_Val = 0,
            Message => "System call to gnatpp returned " & Ret_Val'Image);
         declare
            Current_Content : constant String :=
              Get_String_From_File (Filename);
            Current_Last_Char : constant Character :=
              Current_Content (Current_Content'Last);
         begin
            if Current_Last_Char /= Original_Last_Char then
               --  correct GNATPP bug (additional LF at end of file)
               Write_String_To_File
                 (Current_Content
                    (Current_Content'First .. Current_Content'Last - 1),
                  Filename);
            end if;
         end;
      end;
      Remove_Cr_Cr_Lf (Filename);
   end Pretty_Print_Sections;

   procedure Remove_Pretty_Print_Flags (Filename : String) is
      Contents     : constant String := Get_String_From_File (Filename);
      New_Contents : constant String :=
        Replace_All
          (Replace_All
             (Replace_All
                (Replace_All (Contents, Pretty_Print_On, ""),
                 Alt_Pretty_Print_On, ""),
              Pretty_Print_Off, ""),
           Alt_Pretty_Print_Off, "");
   begin
      Write_String_To_File (New_Contents, Filename);
   end Remove_Pretty_Print_Flags;

end Rejuvenation.Pretty_Print;
