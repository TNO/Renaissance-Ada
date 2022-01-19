with Ada.Directories; use Ada.Directories;

package Rejuvenation.File_Utils is

   procedure Write_String_To_File (Contents : String; File_Name : String);
   --  Write the Contents string to a file with the given File_Name.

   function Get_String_From_File (File_Name : String) return String;
   --  Return the contents string of a file with the given File_Name.

   procedure Walk_Files
     (Directory_Name : String; File_Pattern : String;
      Process_File   : not null access procedure (Item : Directory_Entry_Type);
      Recursive      : Boolean := True);
   --  Apply the procedure to all files that match the pattern
   --  in the given directory.
   --  The Recursive flags determines whether subdirectory are also include.

end Rejuvenation.File_Utils;
