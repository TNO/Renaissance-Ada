with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Commands;              use Commands;

package body SVN_Version_Controls is

   overriding procedure Create_Patch
     (S : SVN_Version_Control; File_Name : String)
   is
   begin
      Execute_Command ("svn diff " & S.Get_Root_Directory & " > " & File_Name);
   end Create_Patch;

   overriding procedure Rewind_Not_Committed_Changes (S : SVN_Version_Control)
   is
   begin
      Execute_Command
        ("svn revert --recursive " & S.Get_Root_Directory & No_Output);
   end Rewind_Not_Committed_Changes;

   overriding function Is_Under_Version_Control
     (S : SVN_Version_Control; File_Name : String) return Boolean
     --  TODO:  investigate ways to speed up
   is
      Command : constant String := "svn info " & File_Name & No_Output;
   begin
      return
        Execute_Command (Command) = 0;
   end Is_Under_Version_Control;

end SVN_Version_Controls;
