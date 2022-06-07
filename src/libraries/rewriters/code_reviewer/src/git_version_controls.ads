with Version_Controls; use Version_Controls;

private with Ada.Strings.Unbounded;

package Git_Version_Controls is

   type Git_Version_Control is new Version_Control with private;

   overriding function Get_Root_Directory
     (G : Git_Version_Control) return String;

   overriding procedure Create_Patch
     (G : Git_Version_Control; File_Name : String);

   overriding procedure Rewind_Not_Committed_Changes (G : Git_Version_Control);

   overriding function Is_Under_Version_Control
     (G : Git_Version_Control; File_Name : String) return Boolean;

   function Make_Git_Version_Control
     (Directory_Name : String) return Git_Version_Control;

private

   type Git_Version_Control is new Version_Control with record
      F_Root_Directory : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Root_Directory
     (G : Git_Version_Control) return String is
      (Ada.Strings.Unbounded.To_String (G.F_Root_Directory));

   function Make_Git_Version_Control
     (Directory_Name : String) return Git_Version_Control is
     (F_Root_Directory =>
        Ada.Strings.Unbounded.To_Unbounded_String (Directory_Name));

end Git_Version_Controls;
