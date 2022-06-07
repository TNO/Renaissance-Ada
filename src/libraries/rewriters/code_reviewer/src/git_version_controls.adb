with Commands;              use Commands;

package body Git_Version_Controls is

   function Go_To_Directory_Command
     (V_C : Version_Control'Class) return String;
   function Go_To_Directory_Command
     (V_C : Version_Control'Class) return String is
     ("cd """ & V_C.Get_Root_Directory & """");

   overriding procedure Create_Patch
     (G : Git_Version_Control; File_Name : String)
   is
   begin
      Execute_Command
        (Go_To_Directory_Command (G) & "& git diff > " & File_Name);
   end Create_Patch;

   overriding procedure Rewind_Not_Committed_Changes (G : Git_Version_Control)
   is
   begin
      Execute_Command
        (Go_To_Directory_Command (G) & "& git restore * " & No_Output);
   end Rewind_Not_Committed_Changes;

   overriding function Is_Under_Version_Control
     (G : Git_Version_Control; File_Name : String) return Boolean
   is
   begin
      return
        Execute_Command
          (Go_To_Directory_Command (G) & "& git ls-files --error-unmatch " &
           File_Name & No_Output) =
        0;
   end Is_Under_Version_Control;

end Git_Version_Controls;
