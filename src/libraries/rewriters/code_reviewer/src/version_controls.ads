package Version_Controls is

   type Version_Control is interface;

   function Get_Root_Directory
     (V_C : Version_Control) return String is abstract;

   procedure Create_Patch
     (V_C : Version_Control; File_Name : String) is abstract;
   --  Create patch

   procedure Rewind_Not_Committed_Changes (V_C : Version_Control) is abstract;

   function Is_Under_Version_Control
     (V_C : Version_Control; File_Name : String) return Boolean is abstract;

end Version_Controls;
