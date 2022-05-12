with Ada.Assertions;                  use Ada.Assertions;
with Ada.Containers;                  use Ada.Containers;
with Ada.Text_IO;                     use Ada.Text_IO;
with Langkit_Support.Text;            use Langkit_Support.Text;
with Libadalang.Common;               use Libadalang.Common;
with Rejuvenation.Match_Patterns;     use Rejuvenation.Match_Patterns;
with Rejuvenation.Node_Locations;     use Rejuvenation.Node_Locations;
with Rejuvenation.Text_Rewrites;      use Rejuvenation.Text_Rewrites;
with Rewriters_Find_And_Replace;      use Rewriters_Find_And_Replace;
with Rewriters_Repeat;                use Rewriters_Repeat;
with Rewriters_Sequence;              use Rewriters_Sequence;
with Rewriters_Vectors;               use Rewriters_Vectors;
with Match_Accepters_Function_Access; use Match_Accepters_Function_Access;

package body Mark_Utils is

   Comment_Open  : constant String := "--";
   Comment_Close : constant String := (1 => ASCII.LF);

   Mark_Open_Comment_Text  : constant String := " { --";
   Mark_Close_Comment_Text : constant String := " } --";

   Mark_Open : constant String :=
     ASCII.LF & Comment_Open & Mark_Open_Comment_Text & Comment_Close;
   Mark_Close : constant String :=
     ASCII.LF & Comment_Open & Mark_Close_Comment_Text & Comment_Close;
   --  for readability Mark Open and close are placed on a new line

   procedure Mark (Unit : in out Analysis_Unit; Nodes : Node_List.Vector) is
      --  Since rewriters will only make changes to marked nodes
      --  including their children, we could remove children from
      --  the vector of nodes to be marked.
      --  Currently, unclear whether that is beneficial, so not done.
      T_R : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (Unit);
   begin
      for Node of Nodes loop
         T_R.Prepend (Node, Mark_Open, All_Trivia);
         T_R.Append (Node, Mark_Close, All_Trivia);
      end loop;
      T_R.Apply;
      Unit.Reparse;
   end Mark;

   function Is_Marked (Node : Ada_Node) return Boolean is

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
      Return_Value : Boolean;
   begin
      Put_Line ("### Is_Some_Parent_Marked In " & Image (Node.Full_Sloc_Image));
      if With_Self and then Is_Marked (Node) then
         Return_Value := True;
      else
         declare
            Parent : constant Ada_Node := Node.Parent;
         begin
            Return_Value :=
              not Parent.Is_Null
              and then Is_Some_Parent_Marked (Parent);
         end;
      end if;
      Put_Line ("### Is_Some_Parent_Marked Out ");
      return Return_Value;
   end Is_Some_Parent_Marked;

end Mark_Utils;
