with Ada.Strings;          use Ada.Strings;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Rejuvenation.Node_Locations is
   --  We postulate that a ghost node is not surrounded by trivia.
   --  A ghost node has a range from X to X-1,
   --  where X is the start position of the next token.
   --  The Text corresponding to a ghost node is, of couse, the empty string.
   --  See also https://gt3-prod-2.adacore.com/#/tickets/UA12-003

   function Node_Start_Offset (Node : Ada_Node'Class) return Positive with
      Pre => not Is_Ghost (Node);

   function Node_Start_Offset (Node : Ada_Node'Class) return Positive is
   begin
      return Raw_Data (Node.Token_Start).Source_First;
   end Node_Start_Offset;

   function Line_Start_Offset (Node : Ada_Node'Class) return Positive with
      Pre => not Is_Ghost (Node);

   function Line_Start_Offset (Node : Ada_Node'Class) return Positive is
      function Line_Start_Offset
        (Token : Token_Reference; Offset : Integer) return Integer;
      --  In Ada comment is ended by a line feed
      --  So a token cannot be preceeded by a comment on the same line
      function Line_Start_Offset
        (Token : Token_Reference; Offset : Integer) return Integer
      is
      begin
         if Token /= No_Token and then Kind (Data (Token)) = Ada_Whitespace
         then
            declare
               Token_Text : constant String :=
                 Encode (Text (Token), Node.Unit.Get_Charset);
               Pos : constant Natural :=
                 Index (Token_Text, (1 => ASCII.LF), Going => Backward);
            begin
               if Pos = 0 then
                  return
                    Line_Start_Offset
                      (Previous (Token), Offset - Token_Text'Length);
               else
                  return
                    Raw_Data (Token).Source_First + Pos + 1 - Token_Text'First;
               end if;
            end;
         else
            return Offset;
         end if;
      end Line_Start_Offset;

   begin
      return
        Line_Start_Offset
          (Previous (Node.Token_Start), Node_Start_Offset (Node));
   end Line_Start_Offset;

   function Trivia_Start_Offset (Node : Ada_Node'Class) return Positive with
      Pre => not Is_Ghost (Node);

   function Trivia_Start_Offset (Node : Ada_Node'Class) return Positive is
      function Trivia_Start_Offset
        (Token : Token_Reference; Nr : Integer) return Integer;
      function Trivia_Start_Offset
        (Token : Token_Reference; Nr : Integer) return Integer
      is
      begin
         if Token /= No_Token
           and then Kind (Data (Token)) in Ada_Whitespace | Ada_Comment
         then
            return
              Trivia_Start_Offset
                (Previous (Token), Raw_Data (Token).Source_First);
         else
            return Nr;
         end if;
      end Trivia_Start_Offset;

   begin
      return
        Trivia_Start_Offset
          (Previous (Node.Token_Start), Node_Start_Offset (Node));
   end Trivia_Start_Offset;

   function Start_Offset
     (Node : Ada_Node'Class; Before : Node_Location := No_Trivia)
      return Positive
   is
   begin
      if Is_Ghost (Node) then
         return Raw_Data (Node.Token_Start).Source_First;
      else
         case Before is
            when No_Trivia =>
               return Node_Start_Offset (Node);
            when Trivia_On_Same_Line =>
               return Line_Start_Offset (Node);
            when All_Trivia =>
               return Trivia_Start_Offset (Node);
         end case;
      end if;
   end Start_Offset;

   function Node_End_Offset (Node : Ada_Node'Class) return Natural with
      Pre => not Is_Ghost (Node);

   function Node_End_Offset (Node : Ada_Node'Class) return Natural is
   begin
      return Raw_Data (Node.Token_End).Source_Last;
   end Node_End_Offset;

   function Line_End_Offset (Node : Ada_Node'Class) return Natural with
     Pre => not Is_Ghost (Node);

   function Line_End_Offset (Node : Ada_Node'Class) return Natural is

      function Line_End_Offset
        (Token : Token_Reference; Offset : Integer) return Integer;
      function Line_End_Offset
        (Token : Token_Reference; Offset : Integer) return Integer
      is
      begin
         if Token /= No_Token
           and then Kind (Data (Token)) in Ada_Whitespace | Ada_Comment
         then
            declare
               Token_Text : constant String :=
                 Encode (Text (Token), Node.Unit.Get_Charset);
               Pos : constant Natural := Index (Token_Text, (1 => ASCII.LF));
            begin
               if Pos = 0 then
                  return
                    Line_End_Offset (Next (Token), Offset + Token_Text'Length);
               else
                  return
                    Raw_Data (Token).Source_First + Pos - Token_Text'First;
               end if;
            end;
         else
            return Offset;
         end if;
      end Line_End_Offset;

   begin
      return Line_End_Offset (Next (Node.Token_End), Node_End_Offset (Node));
   end Line_End_Offset;

   function Trivia_End_Offset (Node : Ada_Node'Class) return Natural with
     Pre => not Is_Ghost (Node);

   function Trivia_End_Offset (Node : Ada_Node'Class) return Natural
   is
      function Trivia_End_Offset
        (Token : Token_Reference; Nr : Natural) return Natural;
      function Trivia_End_Offset
        (Token : Token_Reference; Nr : Natural) return Natural
      is
      begin
         if Token /= No_Token
           and then Kind (Data (Token)) in Ada_Whitespace | Ada_Comment
         then
            return
              Trivia_End_Offset (Next (Token), Raw_Data (Token).Source_Last);
         else
            return Nr;
         end if;
      end Trivia_End_Offset;

   begin
      return Trivia_End_Offset (Next (Node.Token_End), Node_End_Offset (Node));
   end Trivia_End_Offset;

   function End_Offset
     (Node : Ada_Node'Class; After : Node_Location := No_Trivia) return Natural
   is
   begin
      if Is_Ghost (Node) then
         return Raw_Data (Node.Token_Start).Source_First - 1;
      else
         case After is
            when No_Trivia =>
               return Node_End_Offset (Node);
            when Trivia_On_Same_Line =>
               return Line_End_Offset (Node);
            when All_Trivia =>
               return Trivia_End_Offset (Node);
         end case;
      end if;
   end End_Offset;

end Rejuvenation.Node_Locations;
