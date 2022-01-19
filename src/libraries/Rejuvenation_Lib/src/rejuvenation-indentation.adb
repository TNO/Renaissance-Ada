with Ada.Assertions;       use Ada.Assertions;
with Ada.Strings;          use Ada.Strings;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Rejuvenation.Indentation is

   function Node_On_Separate_Lines (Node : Ada_Node'Class) return Boolean is

      function First_Token_On_Line (Token : Token_Reference) return Boolean;

      function First_Token_On_Line (Token : Token_Reference) return Boolean is

         function Previous_Token_Not_On_Line
           (Token : Token_Reference) return Boolean;

         function Previous_Token_Not_On_Line
           (Token : Token_Reference) return Boolean
         is
         begin
            if Token = No_Token then
               return True;
            elsif Kind (Data (Token)) = Ada_Whitespace then
               declare
                  Token_Text : constant String :=
                    Encode (Text (Token), Node.Unit.Get_Charset);
               begin
                  return
                    Index (Token_Text, (1 => ASCII.LF)) > 0
                    or else Previous_Token_Not_On_Line (Previous (Token));
               end;
            else
               Assert
                 (Check   => Kind (Data (Token)) /= Ada_Comment,
                  Message =>
                    "Comment is terminated by line end. " &
                    "So no token can follow it on the same line");
               return False;
            end if;
         end Previous_Token_Not_On_Line;
      begin
         return Previous_Token_Not_On_Line (Previous (Token));
      end First_Token_On_Line;

      function Last_Token_On_Line (Token : Token_Reference) return Boolean;

      function Last_Token_On_Line (Token : Token_Reference) return Boolean is

         function Next_Token_Not_On_Line
           (Token : Token_Reference) return Boolean;

         function Next_Token_Not_On_Line
           (Token : Token_Reference) return Boolean
         is
         begin
            case Kind (Data (Token)) is
               when Ada_Termination =>
                  return True;
               when Ada_Comment =>
                  return Next_Token_Not_On_Line (Next (Token));
                  --  really checking on new line might not be needed,
                  --  since comment in Ada is always terminated by a new line.
               when Ada_Whitespace =>
                  declare
                     Token_Text : constant String :=
                       Encode (Text (Token), Node.Unit.Get_Charset);
                  begin
                     return
                       Index (Token_Text, (1 => ASCII.LF)) > 0
                       or else Next_Token_Not_On_Line (Next (Token));
                  end;
               when others =>
                  return False;
            end case;
         end Next_Token_Not_On_Line;
      begin
         return Next_Token_Not_On_Line (Next (Token));
      end Last_Token_On_Line;

   begin
      return
        First_Token_On_Line (Node.Token_Start)
        and then Last_Token_On_Line (Node.Token_End);
   end Node_On_Separate_Lines;

   function Indentation_Of_Node (Node : Ada_Node'Class) return Integer is
      function Indentatation_Of_Token
        (Token : Token_Reference; Nr : Integer) return Integer;
      function Indentatation_Of_Token
        (Token : Token_Reference; Nr : Integer) return Integer
      is
      begin
         if Token = No_Token then
            return Nr;
         elsif Kind (Data (Token)) = Ada_Whitespace then
            declare
               Token_Text : constant String :=
                 Encode (Text (Token), Node.Unit.Get_Charset);
               Pos : constant Natural :=
                 Index (Token_Text, (1 => ASCII.LF), Going => Backward);
            begin
               if Pos = 0 then
                  return
                    Indentatation_Of_Token
                      (Previous (Token), Nr + Token_Text'Length);
               else
                  return Nr + Token_Text'Last - Pos;
               end if;
            end;
         else
            return No_Indentation;
         end if;
      end Indentatation_Of_Token;

   begin
      return Indentatation_Of_Token (Previous (Node.Token_Start), 0);
   end Indentation_Of_Node;

end Rejuvenation.Indentation;
