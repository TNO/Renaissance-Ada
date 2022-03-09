with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
with Langkit_Support.Text;  use Langkit_Support.Text;

package body Rejuvenation.Utils is

   -- Raw signature --------

   function Raw_Signature
     (Node : Ada_Node'Class; Before, After : Node_Location := No_Trivia)
      return String
   is
   begin
      if Node.Is_Null then
         return "";
      else
         declare
            First    : constant Positive      := Start_Offset (Node, Before);
            Last     : constant Natural       := End_Offset (Node, After);
            Unit     : constant Analysis_Unit := Node.Unit;
            Node_Str : constant String        :=
              Encode (Unit.Text (First .. Last), Unit.Get_Charset);
         begin
            return Node_Str;
         end;
      end if;
   end Raw_Signature;

   function Raw_Signature
     (Token : Token_Reference; Charset : String) return String is
     (if Token = No_Token then "" else Encode (Text (Token), Charset));

   function Raw_Signature
     (First_Node, Last_Node : Ada_Node'Class;
      Before, After         : Node_Location := No_Trivia) return String
   is
      First : constant Positive      := Start_Offset (First_Node, Before);
      Last  : constant Natural       := End_Offset (Last_Node, After);
      Unit  : constant Analysis_Unit := First_Node.Unit;
      Str   : constant String        :=
        Encode (Unit.Text (First .. Last), Unit.Get_Charset);
   begin
      return Str;
   end Raw_Signature;

   function Are_Equal_As_Raw_Signature
     (Node1, Node2 : Ada_Node'Class) return Boolean
   is
   begin
      return Raw_Signature (Node1) = Raw_Signature (Node2);
   end Are_Equal_As_Raw_Signature;

   function Are_Equal_Case_Insensitive_As_Raw_Signature
     (Node1, Node2 : Ada_Node'Class) return Boolean
   is
   begin
      return
        Equal_Case_Insensitive (Raw_Signature (Node1), Raw_Signature (Node2));
   end Are_Equal_Case_Insensitive_As_Raw_Signature;

   function Are_Equal_In_Ada (Node1, Node2 : Ada_Node'Class) return Boolean
   is
   begin
      if Node1.Is_Null or else Node2.Is_Null then
         return Node1.Is_Null = Node2.Is_Null;
      end if;

      if Node1.Kind /= Node2.Kind then
         return False;
      end if;

      if Node1.Children_Count /= Node2.Children_Count then
         return False;
      end if;

      if Node1.Children_Count = 0 then
         case Node1.Kind is
            when Ada_String_Literal | Ada_Char_Literal =>
               return Are_Equal_As_Raw_Signature (Node1, Node2);
            when Ada_Int_Literal =>
               declare
                  Int_Literal1 : constant Int_Literal :=
                    Node1.As_Int_Literal;
                  Int_Literal2 : constant Int_Literal :=
                    Node2.As_Int_Literal;
               begin
                  return Int_Literal1.P_Denoted_Value =
                    Int_Literal2.P_Denoted_Value;
               end;
            --  TODO: Add semantic equal Real_Literal
            when others =>
               return
                 Are_Equal_Case_Insensitive_As_Raw_Signature (Node1, Node2);
         end case;
      else
         return (for all Index in 1 .. Node1.Children_Count =>
                   Are_Equal_In_Ada (Node1.Child (Index), Node2.Child (Index))
                );
      end if;

   end Are_Equal_In_Ada;

   --  Package (Distributed over files) functionality

   function In_Same_Package (Unit1, Unit2 : Analysis_Unit) return Boolean is
      Unit1_Filename : constant String := Unit1.Get_Filename;
      Unit2_Filename : constant String := Unit2.Get_Filename;
   begin
      --  TODO: should the comparison be case insensitive?
      return
        Unit1_Filename (Unit1_Filename'First .. Unit1_Filename'Last - 1) =
        Unit2_Filename (Unit2_Filename'First .. Unit2_Filename'Last - 1);
   end In_Same_Package;

   -- Image --------

   function Image (Node_List_Vector : Node_List.Vector) return String is
   begin
      if Node_List_Vector.Length = 0 then
         return "[]";
      else
         declare
            Str : Unbounded_String :=
              To_Unbounded_String (Node_List_Vector.First_Element.Image);
         begin
            for Index in
              Node_List_Vector.First_Index + 1 .. Node_List_Vector.Last_Index
            loop
               Str :=
                 Str & ", " &
                 Node_List_Vector.Element (Positive (Index)).Image;
            end loop;
            return "[" & To_String (Str) & "]";
         end;
      end if;
   end Image;

   -- Get trivia tokens --------

   function Get_Trivia_Before (Node : Ada_Node'Class) return Token_List.Vector
   is
   begin
      return Get_Trivia_Before (Node.Token_Start);
   end Get_Trivia_Before;

   function Get_Trivia_Before
     (Token : Token_Reference) return Token_List.Vector
   is
      Results       : Token_List.Vector;
      Running_Token : Token_Reference := Previous (Token);
   begin
      while Is_Trivia (Running_Token) loop
         Token_List.Prepend (Results, Running_Token);
         Running_Token := Previous (Running_Token);
      end loop;
      return Results;
   end Get_Trivia_Before;

   function Get_Trivia_After (Node : Ada_Node'Class) return Token_List.Vector
   is
   begin
      return Get_Trivia_After (Node.Token_End);
   end Get_Trivia_After;

   function Get_Trivia_After (Token : Token_Reference) return Token_List.Vector
   is
      Results       : Token_List.Vector;
      Running_Token : Token_Reference := Next (Token);
   begin
      while Is_Trivia (Running_Token) loop
         Token_List.Append (Results, Running_Token);
         Running_Token := Next (Running_Token);
      end loop;
      return Results;
   end Get_Trivia_After;

   function Get_Tokens (Node : Ada_Node'Class) return Token_List.Vector is
      Results       : Token_List.Vector;
      Running_Token : Token_Reference := Node.Token_Start;
   begin
      while Running_Token /= Node.Token_End loop
         Token_List.Append (Results, Running_Token);
         Running_Token := Next (Running_Token);
      end loop;
      Token_List.Append (Results, Running_Token);
      return Results;
   end Get_Tokens;

end Rejuvenation.Utils;
