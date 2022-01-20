with Ada.Text_IO; use Ada.Text_IO;

package body NewLineExamples is

   function Text_New_Lines (Text : String) return String
   is
   begin
      return Text & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
   end Text_New_Lines;

   function Twice_Text_New_Line (Text : String) return String
   is
   begin
      return Text & ASCII.CR & ASCII.LF & Text & ASCII.CR & ASCII.LF;
   end Twice_Text_New_Line;

   Nl : constant String := (1 => ASCII.CR, 2 => ASCII.LF);

   function Text_Duplicate (Text : String) return String
   is
   begin
      return Text & Nl & Text & Nl;
   end Text_Duplicate;

   function Text_Dupl (Text : String) return String
   is
   begin
      declare
         EndOfLine : constant String := ASCII.CR & ASCII.LF;
      begin
         return Text & EndOfLine & Text & EndOfLine;
      end;
   end Text_Dupl;

   function Text_Twice (Text : String) return String
   is
      CrLf : constant String := ASCII.CR & ASCII.LF;
   begin
      return Text & CrLf & Text & CrLf;
   end Text_Twice;

   function Text_Thrice (Text : String) return String
   is
      NewLine : constant String := (ASCII.CR & ASCII.LF);
   begin
      return Text & NewLine & Text & NewLine & Text & NewLine;
   end Text_Thrice;

   function Twice_Text (Text : String) return String
   is
      New_Line : constant String := "" & ASCII.CR & ASCII.LF;
   begin
      return New_Line & Text & New_Line & Text;
   end Twice_Text;

end NewLineExamples;
