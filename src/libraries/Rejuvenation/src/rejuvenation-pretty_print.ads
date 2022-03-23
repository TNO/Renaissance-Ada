with Ada.Directories;            use Ada.Directories;
with Rejuvenation.Text_Rewrites; use Rejuvenation.Text_Rewrites;

package Rejuvenation.Pretty_Print is
   --  TODO: what if we only want to insert (append/prepend) code that
   --  should be pretty printed?
   --  In that case we have no node (yet) to surround
   --  with pretty print section!

   --  This functionality support the following flow
   --  1. Enter flags to specify exactly those sections that should be
   --     affected by pretty printing.
   --  2. Enter flag at the start of the file to turn pretty printing
   --     initially off.
   --  3. Pretty print the sections in the file
   --  4. Remove the flags related to this functionality
   --
   --  Note: Nesting of pretty print sections is allowed (unlike by GNATpp).
   --
   --  Note: Since the file might already contain user defined
   --        pretty print sections, the provided flags are made unique
   --        for this functionality.
   --
   --  See https://gt3-prod-1.adacore.com/#/tickets/U722-009 :
   --  GNATpp requires that the flags must appear on a line by themselves, with
   --  nothing preceding except spaces
   --
   --  To ensure that the pretty print flags appear on a line by themselves,
   --  with nothing preceding except spaces,
   --  users can't access the flags directly.

   procedure Surround_Node_By_Pretty_Print_Section
     (T_R : in out Text_Rewrite'Class; Node : Ada_Node'Class);
   --  Execute step 1: Surround Node by Pretty Print Section.
   --  This function can be called multiple times to surrounded
   --  multiple nodes by pretty print sections.

   procedure Turn_Pretty_Printing_Initially_Off
     (T_R : in out Text_Rewrite_Unit);
   --  Execute step 2: Turn pretty print off at start of Text Rewrite
   --  A Turn_Pretty_Printing_Initially_Off function should be
   --  called exactly once.

   procedure Turn_Pretty_Printing_Initially_Off (Filename : String);
   --  Execute step 2: Turn pretty print off at start of file
   --  A Turn_Pretty_Printing_Initially_Off function should be
   --  called exactly once.

   Standard_Options_Project : constant String;
   --  Reference to the standard options project (of Nexperia)

   procedure Pretty_Print_Sections (Filename : String; Projectname : String);
   --  Execute step 3: Pretty print sections between the pretty print flags
   --  This function should be called exactly once.

   procedure Remove_Pretty_Print_Flags (Filename : String);
   --  Execute step 4: Remove pretty print flags
   --  This function should be called exactly once.

private

   Standard_Options_Project : constant String :=
     Compose ("C:\path\to\Renaissance-Ada\src\libraries\Standard_Options",
              "standard_options", "gpr");

   Preemable : constant String := "--";
   Flag_On   : constant String := "!rej_on";
   Flag_Off  : constant String := "!rej_off";
   Lf        : constant String := (1 => ASCII.LF);

   Pretty_Print_On : constant String := Preemable & Flag_On & Lf;
   --  flag to turn PRETTY PRINT on
   Pretty_Print_Off : constant String := Preemable & Flag_Off & Lf;
   --  flag to turn PRETTY PRINT off

   --  needed to work around bugs in gnatpp:
   --  an alternative postamble using Cr_Lf iso Lf
   Cr_Lf                : constant String := ASCII.CR & ASCII.LF;
   Alt_Pretty_Print_On  : constant String := Preemable & Flag_On & Cr_Lf;
   Alt_Pretty_Print_Off : constant String := Preemable & Flag_Off & Cr_Lf;

end Rejuvenation.Pretty_Print;
