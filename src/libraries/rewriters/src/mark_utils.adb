with Ada.Assertions;                  use Ada.Assertions;
with Ada.Containers;                  use Ada.Containers;
with Langkit_Support.Text;            use Langkit_Support.Text;
with Libadalang.Common;               use Libadalang.Common;
with Rejuvenation.File_Utils;         use Rejuvenation.File_Utils;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Node_Locations;     use Rejuvenation.Node_Locations;
with Rejuvenation.Pretty_Print;       use Rejuvenation.Pretty_Print;
with Rejuvenation.String_Utils;       use Rejuvenation.String_Utils;
with Rejuvenation.Text_Rewrites;      use Rejuvenation.Text_Rewrites;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Repeat;                use Rewriters_Repeat;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Combine;         use Match_Accepters_Combine;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;
with Match_Accepters_Marked;          use Match_Accepters_Marked;

package body Mark_Utils is

   Comment_Close : constant String := (1 => ASCII.LF);

   Mark_Open_Comment_Text  : constant String := "-- { --";
   Mark_Close_Comment_Text : constant String := "-- } --";
   --  Libadalang includes `--` Comment Open in the Comment Text

   Mark_Open : constant String :=
     ASCII.LF & Mark_Open_Comment_Text & Comment_Close;
   Mark_Close : constant String :=
     ASCII.LF & Mark_Close_Comment_Text & Comment_Close;
   --  for readability Mark Open and Close are placed on a new line

   function Add_Marks_And_Pretty_Print_Sections
     (Unit : in out Analysis_Unit; Nodes : Node_List.Vector)
           return Boolean
   is
      --  Since rewriters will only make changes to marked nodes
      --  including their children / descendants,
      --  we could remove children / descendants from
      --  the vector of nodes to be marked.
      --  Currently, unclear whether that is beneficial, so not done.

      --  Since Marks and Pretty Print Sections interact
      --  e.g. Marks places nodes on separate lines
      --  they must be added together.
   begin
      if Nodes.Is_Empty then
         return False;
      else
         declare
            T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
         begin
            Turn_Pretty_Printing_Initially_Off (T_R);
            for Node of Nodes loop
               T_R.Prepend (Node, Mark_Open);
               T_R.Append (Node, Mark_Close);
               Surround_Node_By_Pretty_Print_Section (T_R, Node);
            end loop;
            T_R.Apply;
            Unit.Reparse;
            return True;
         end;
      end if;
   end Add_Marks_And_Pretty_Print_Sections;

   procedure Remove_Marks (Filename : String) is
      Contents     : constant String := Get_String_From_File (Filename);
      New_Contents : constant String :=
        Replace_All (Replace_All (Contents, Mark_Open, ""), Mark_Close, "");
   begin
      Write_String_To_File (New_Contents, Filename);
   end Remove_Marks;

   function Is_Marked (Node : Ada_Node'Class) return Boolean is

      function Has_Mark_Open_Before (Token : Token_Reference) return Boolean;
      function Has_Mark_Open_Before (Token : Token_Reference) return Boolean is
      begin
         if Token = No_Token then
            return False;
         else
            case Kind (Data (Token)) is
               when Ada_Whitespace =>
                  return Has_Mark_Open_Before (Previous (Token));
               when Ada_Comment =>
                  declare
                     Token_Text : constant String :=
                       Encode (Text (Token), Node.Unit.Get_Charset);
                  begin
                     return
                       Token_Text = Mark_Open_Comment_Text
                       or else Has_Mark_Open_Before (Previous (Token));
                  end;
               when others =>
                  return False;
            end case;
         end if;
      end Has_Mark_Open_Before;

      function Has_Mark_Close_After (Token : Token_Reference) return Boolean;
      function Has_Mark_Close_After (Token : Token_Reference) return Boolean is
      begin
         if Token = No_Token then
            return False;
         else
            case Kind (Data (Token)) is
               when Ada_Whitespace =>
                  return Has_Mark_Close_After (Next (Token));
               when Ada_Comment =>
                  declare
                     Token_Text : constant String :=
                       Encode (Text (Token), Node.Unit.Get_Charset);
                  begin
                     return
                       Token_Text = Mark_Close_Comment_Text
                       or else Has_Mark_Close_After (Next (Token));
                  end;
               when others =>
                  return False;
            end case;
         end if;
      end Has_Mark_Close_After;

   begin
      return
        Has_Mark_Open_Before (Previous (Node.Token_Start))
        and then Has_Mark_Close_After (Next (Node.Token_End));
   end Is_Marked;

   function Is_Some_Parent_Marked
     (Node : Ada_Node; With_Self : Boolean := True) return Boolean
   is
   begin
      if With_Self and then Is_Marked (Node) then
         return True;
      else
         declare
            Parent : constant Ada_Node := Node.Parent;
         begin
            return not Parent.Is_Null and then Is_Some_Parent_Marked (Parent);
         end;
      end if;
   end Is_Some_Parent_Marked;

   function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace) return Rewriter_Find_And_Replace;
   function Make_Rewriter_Find_And_Replace_Mark_Aware
     (R : Rewriter_Find_And_Replace) return Rewriter_Find_And_Replace
   is
   begin
      return
        Make_Rewriter_Find_And_Replace
          (R.Get_Find_Pattern, R.Get_Replace_Pattern,
           Make_Match_Accepter_Combine
             (R.Get_Match_Accepter, Make_Match_Accepter_Marked));
   end Make_Rewriter_Find_And_Replace_Mark_Aware;

   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat) return Rewriter_Repeat;
   function Make_Rewriter_Repeat_Mark_Aware
     (R : Rewriter_Repeat) return Rewriter_Repeat
   is
   begin
      return Make_Rewriter_Repeat (Make_Rewriter_Mark_Aware (R.Get_Rewriter));
   end Make_Rewriter_Repeat_Mark_Aware;

   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence) return Rewriter_Sequence;
   function Make_Rewriter_Sequence_Mark_Aware
     (R : Rewriter_Sequence) return Rewriter_Sequence
   is
      Return_Value : Rewriters_Vectors.Vector;
   begin
      for Rewriter of R.Get_Vector loop
         Return_Value.Append (Make_Rewriter_Mark_Aware (Rewriter));
      end loop;
      return Make_Rewriter_Sequence (Return_Value);
   end Make_Rewriter_Sequence_Mark_Aware;

   function Make_Rewriter_Mark_Aware (R : Rewriter'Class) return Rewriter'Class
   is
   begin
      if R in Rewriter_Find_And_Replace'Class then
         return
           Make_Rewriter_Find_And_Replace_Mark_Aware
             (Rewriter_Find_And_Replace (R));
      elsif R in Rewriter_Repeat'Class then
         return Make_Rewriter_Repeat_Mark_Aware (Rewriter_Repeat (R));
      elsif R in Rewriter_Sequence'Class then
         return Make_Rewriter_Sequence_Mark_Aware (Rewriter_Sequence (R));
      else
         Assert (False, "Unknown Rewriter class");
         return R;
      end if;
   end Make_Rewriter_Mark_Aware;

end Mark_Utils;
