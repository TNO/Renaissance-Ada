with Version_Controls; use Version_Controls;

private with Ada.Strings.Unbounded;

package SVN_Version_Controls is

   type SVN_Version_Control is new Version_Control with private;

   overriding function Get_Root_Directory
     (S : SVN_Version_Control) return String;

   overriding procedure Create_Patch
     (S : SVN_Version_Control; File_Name : String);

   overriding procedure Rewind_Not_Committed_Changes (S : SVN_Version_Control);

   overriding function Is_Under_Version_Control
     (S : SVN_Version_Control; File_Name : String) return Boolean;

   function Make_SVN_Version_Control
     (Directory_Name : String) return SVN_Version_Control;

private

   type SVN_Version_Control is new Version_Control with record
      F_Root_Directory : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Root_Directory
     (S : SVN_Version_Control) return String is
      (Ada.Strings.Unbounded.To_String (S.F_Root_Directory));

   function Make_SVN_Version_Control
     (Directory_Name : String) return SVN_Version_Control is
     (F_Root_Directory =>
        Ada.Strings.Unbounded.To_Unbounded_String (Directory_Name));

end SVN_Version_Controls;
