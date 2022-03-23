with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Rejuvenation.File_Utils is

   procedure Write_String_To_File (Contents : String; File_Name : String) is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Create_Path (Containing_Directory (File_Name));
      Create (F, Out_File, File_Name);
      String'Write (Stream (F), Contents);
      Close (F);
   end Write_String_To_File;

   function Get_String_From_File (File_Name : String) return String is
      F            : Ada.Streams.Stream_IO.File_Type;
      Return_Value : Unbounded_String;
   begin
      Open (F, In_File, File_Name);
      while not End_Of_File (F) loop
         declare
            C : Character;
         begin
            Character'Read (Stream (F), C);
            Append (Return_Value, C);
         end;
      end loop;
      Close (F);
      return To_String (Return_Value);
   end Get_String_From_File;

   procedure Walk_Files
     (Directory_Name : String; File_Pattern : String;
      Process_File   : not null access procedure (Item : Directory_Entry_Type);
      Recursive      : Boolean := True)
   is

      procedure Process_Directory (Item : Directory_Entry_Type);
      procedure Process_Directory (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) not in "." | ".." then
            Walk_Files (Full_Name (Item), File_Pattern, Process_File);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end Process_Directory;

   begin
      Search (Directory_Name, File_Pattern, (others => True), Process_File);
      Search
        (Directory_Name, "", (Directory => Recursive, others => False),
         Process_Directory'Access);
   end Walk_Files;

end Rejuvenation.File_Utils;
