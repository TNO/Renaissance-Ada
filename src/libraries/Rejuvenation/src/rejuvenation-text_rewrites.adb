with Ada.Assertions; use Ada.Assertions;
with Ada.Strings;    use Ada.Strings;
--  Debug includes:
--  with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
--  with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Rejuvenation.File_Utils; use Rejuvenation.File_Utils;
with Rejuvenation.Navigation; use Rejuvenation.Navigation;

package body Rejuvenation.Text_Rewrites is

   -- Public: Collect rewrite operation (node) --------
   function Prepend
     (TR     : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before :        Node_Location := No_Trivia; Charset : String := "")
      return Boolean
   is
      First : constant Positive         := Start_Offset (Node, Before);
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      Assert
        (Check   => TR.Is_Initialized,
         Message => "Text Rewrite used uninitialized");
      Assert
        (Check   => Is_Reflexive_Ancestor (TR.Get_Unit.Root, Node),
         Message =>
           "Text Rewrite does not contain provided Node - " &
           Langkit_Support.Text.Image (Node.Text));
      Assert
        (Check   => TR.First <= First,
         Message => "Text Rewrite - prepend before part");
      return Replace (TR, First, First - 1, WText);
   end Prepend;

   function Append
     (TR    : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      After :        Node_Location := No_Trivia; Charset : String := "")
      return Boolean
   is
      Last  : constant Positive         := End_Offset (Node, After);
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      Assert
        (Check   => TR.Is_Initialized,
         Message => "Text Rewrite used uninitialized");
      Assert
        (Check   => Is_Reflexive_Ancestor (TR.Get_Unit.Root, Node),
         Message =>
           "Text Rewrite Node does not contain provided Node - " &
           Langkit_Support.Text.Image (Node.Text));
      Assert
        (Check   => Last <= TR.Last,
         Message => "Text Rewrite - append after part");
      return Replace (TR, Last + 1, Last, WText);
   end Append;

   procedure Remove
     (TR     : in out Text_Rewrite; Node : Ada_Node'Class;
      Before : Node_Location := No_Trivia; After : Node_Location := No_Trivia)
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Remove (Node, Before, After);
   end Remove;

   procedure Prepend
     (TR     : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before :        Node_Location := No_Trivia; Charset : String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Prepend (Node, Text, Before, Charset);
   end Prepend;

   procedure Append
     (TR    : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      After :        Node_Location := No_Trivia; Charset : String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Append (Node, Text, After, Charset);
   end Append;

   procedure Replace
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before, After :    Node_Location := No_Trivia; Charset : String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Replace (Node, Text, Before, After, Charset);
   end Replace;

   procedure Restore (TR : in out Text_Rewrite; Node : Ada_Node'Class) is
      Dummy : Boolean;
   begin
      Dummy := TR.Restore (Node);
   end Restore;

   procedure ReplaceAround
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Before_Text : String;
      Innernode :        Ada_Node'Class; After_Text : String;
      Before : Node_Location := No_Trivia; After : Node_Location := No_Trivia;
      Charset   :        String        := "")
   is
      NodeStartOffset : constant Positive := Start_Offset (Node, Before);
      NodeEndOffset   : constant Positive := End_Offset (Node, After);

      InnernodeStartOffset : constant Positive :=
        Start_Offset (Innernode, Before);
      InnernodeEndOffset : constant Positive := End_Offset (Innernode, After);

      Before_WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Before_Text, Charset);
      After_WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (After_Text, Charset);

      Dummy_Before, Dummy_After : Boolean;
   begin
      Assert
        (Check   => TR.Is_Initialized,
         Message => "Text Rewrite used uninitialized");
      Assert
        (Check   => Is_Reflexive_Ancestor (TR.Get_Unit.Root, Node),
         Message =>
           "Text Rewrite Node does not contain provided Node - " &
           Langkit_Support.Text.Image (Node.Text));
      Assert
        (Check   => TR.First <= NodeStartOffset,
         Message => "Text Rewrite - start of node outside part");
      Assert
        (Check   => NodeEndOffset <= TR.Last,
         Message => "Text Rewrite - end of node outside part");
      Assert
        (Check   => Is_Reflexive_Ancestor (Node, Innernode),
         Message => "Innernode should be contained in node");
      Log ("ReplaceAround - Before");
      Dummy_Before :=
        Replace (TR, NodeStartOffset, InnernodeStartOffset - 1, Before_WText);
      Log ("ReplaceAround - After");
      Dummy_After :=
        Replace (TR, InnernodeEndOffset + 1, NodeEndOffset, After_WText);
      Assert
        (Check   => Dummy_Before = Dummy_After,
         Message =>
           Langkit_Support.Text.Image (Node.Full_Sloc_Image) &
           "How to handle only one of the two is accepted?" & ASCII.LF &
           "Before = " & Boolean'Image (Dummy_Before) & ASCII.LF & "After = " &
           Boolean'Image (Dummy_After));
   end ReplaceAround;

   -- Collect rewrite operation (nodes) ---------------

   function Replace
     (TR    : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Text  :        String; Before : Node_Location := No_Trivia;
      After :        Node_Location := No_Trivia; Charset : String := "")
      return Boolean
   is
      First : constant Positive         := Start_Offset (First_Node, Before);
      Last  : constant Positive         := End_Offset (Last_Node, After);
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      Assert
        (Check   => TR.Is_Initialized,
         Message => "Text Rewrite used uninitialized");
      --  TODO: First and Last node might be the same node
      --  How to check it efficiently?
      --  Options [at least]
      --    * Move Assert upwards (before duplication happens)
      --    * add check "First_Node = Last_Node or else .."
      Assert
        (Check   => Is_Reflexive_Ancestor (TR.Get_Unit.Root, First_Node),
         Message =>
           "Text Rewrite Node does not contain provided First Node - " &
           Langkit_Support.Text.Image (First_Node.Text));
      Assert
        (Check   => TR.First <= First,
         Message => "Text Rewrite - start of first node outside part");
      Assert
        (Check   => Is_Reflexive_Ancestor (TR.Get_Unit.Root, Last_Node),
         Message =>
           "Text Rewrite Node does not contain provided Last Node - " &
           Langkit_Support.Text.Image (Last_Node.Text));
      Assert
        (Check   => Last <= TR.Last,
         Message => "Text Rewrite - end of last node outside part");
      return Replace (TR, First, Last, WText);
   end Replace;

   procedure Remove
     (TR     : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Before : Node_Location := No_Trivia; After : Node_Location := No_Trivia)
   is
      Dummy : Boolean;
   begin
      Dummy := Remove (TR, First_Node, Last_Node, Before, After);
   end Remove;

   procedure Replace
     (TR    : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Text  :        String; Before : Node_Location := No_Trivia;
      After :        Node_Location := No_Trivia; Charset : String := "")
   is
      Dummy : Boolean;
   begin
      Dummy :=
        Replace (TR, First_Node, Last_Node, Text, Before, After, Charset);
   end Replace;

   -- Public: Collect rewrite operation (token) --------

   function Prepend
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean
   is
      First : constant Integer          := Raw_Data (Token).Source_First;
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      return Replace (TR, First, First - 1, WText);
   end Prepend;

   function Append
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean
   is
      Last  : constant Integer          := Raw_Data (Token).Source_Last;
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      return Replace (TR, Last + 1, Last, WText);
   end Append;

   function Replace
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean
   is
      First : constant Integer          := Raw_Data (Token).Source_First;
      Last  : constant Integer          := Raw_Data (Token).Source_Last;
      WText : constant Wide_Wide_String :=
        Langkit_Support.Text.Decode (Text, Charset);
   begin
      return Replace (TR, First, Last, WText);
   end Replace;

   procedure Remove (TR : in out Text_Rewrite; Token : Token_Reference) is
      Dummy : Boolean;
   begin
      Dummy := TR.Remove (Token);
   end Remove;

   procedure Prepend
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Prepend (Token, Text, Charset);
   end Prepend;

   procedure Append
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Append (Token, Text, Charset);
   end Append;

   procedure Replace
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "")
   is
      Dummy : Boolean;
   begin
      Dummy := TR.Replace (Token, Text, Charset);
   end Replace;

   -- Public: Inspect rewrite operations --------

   function HasReplacements (TR : Text_Rewrite) return Boolean is
   begin
      return not TR.Entries.Is_Empty;
   end HasReplacements;

   -- Public: Apply rewrite operations --------

   function ApplyToString (TR : Text_Rewrite) return String is
      use Replacement_List;
      Txt : constant Langkit_Support.Text.Text_Type :=
        TR.Get_Unit.Text (TR.First .. TR.Last);
      Str : Unbounded_Wide_Wide_String := To_Unbounded_Wide_Wide_String (Txt);
      Str_First : constant Positive          := 1;
      --  lower bound of an Unbounded_Wide_Wide_String is 1
      C : Cursor := TR.Entries.Last;
   begin
      Assert
        (Check   => TR.Is_Initialized,
         Message => "Text Rewrite used uninitialized");
      Log ("ApplyToString -" & TR.Entries.Length'Image & " Slices");
      while Has_Element (C) loop
         declare
            RE          : constant Replacement_Entry := Element (C);
            From        : constant Positive := RE.First - TR.First + Str_First;
            To          : constant Natural := RE.Last - TR.First + Str_First;
            Replacement : constant Wide_Wide_String  :=
              To_Wide_Wide_String (RE.Text);
         begin
            Log
              ("Slice #" & To_Index (C)'Image & " [" & From'Image & ":" &
               To'Image & " ]");
            Replace_Slice (Str, From, To, Replacement);
         end;

         Previous (C);
      end loop;

      return
        Langkit_Support.Text.Encode
          (To_Wide_Wide_String (Str), TR.Get_Unit.Get_Charset);
   exception
      when others =>
         Put_Line ("Error in ApplyToString - " & TR.Get_Unit.Get_Filename);
         raise;
   end ApplyToString;

   function Make_Text_Rewrite_Nodes
     (First_Node, Last_Node : Ada_Node'Class;
      Before : Node_Location := No_Trivia; After : Node_Location := No_Trivia)
      return Text_Rewrite
   is
      First : constant Positive := Start_Offset (First_Node, Before);
      Last  : constant Positive := End_Offset (Last_Node, After);
   begin
      Assert
        (Check   => First <= Last,
         Message => "Make_Text_Rewrite_Nodes - not a sequence");
      Assert
        (Check   => "=" (First_Node.Unit, Last_Node.Unit),
         Message => "Make_Text_Rewrite_Nodes - not same unit");

      return
        (Entries => Replacement_List.Empty_Vector, Unit => First_Node.Unit,
         First   => First, Last => Last);
   end Make_Text_Rewrite_Nodes;

   function Make_Text_Rewrite_Unit
     (Unit : Analysis_Unit) return Text_Rewrite_Unit
   is
   begin
      return
        (Entries => Replacement_List.Empty_Vector, Unit => Unit, First => 1,
         Last    => Length (To_Unbounded_Wide_Wide_String (Unit.Text)));
   end Make_Text_Rewrite_Unit;

   procedure Apply (TR : Text_Rewrite_Unit) is
   begin
      if TR.HasReplacements then
         Write_String_To_File (TR.ApplyToString, TR.Get_Unit.Get_Filename);
      end if;
   end Apply;

   procedure Apply_And_Reparse (TR : in out Text_Rewrite_Unit) is
   begin
      if TR.HasReplacements then
         declare
            Unit : constant Analysis_Unit := TR.Get_Unit;
         begin
            Write_String_To_File (TR.ApplyToString, Unit.Get_Filename);
            Unit.Reparse;
            TR := Make_Text_Rewrite_Unit (Unit);
         end;
      end if;
   end Apply_And_Reparse;

   -- Private: Collect rewrite operation --------
   function Replace_Inner
     (TR : in out Text_Rewrite; First, Last : Natural; Text : Wide_Wide_String)
      return Boolean;

   --  Return value indicates whether replacement is accepted.
   function Replace_Inner
     (TR : in out Text_Rewrite; First, Last : Natural; Text : Wide_Wide_String)
      return Boolean
   is
      use Replacement_List;
      New_Text : Unbounded_Wide_Wide_String :=
        To_Unbounded_Wide_Wide_String (Text);
      C : Cursor := TR.Entries.First;
   begin
      while Has_Element (C) loop
         declare
            RE : Replacement_Entry := Element (C);
         begin
            if First = RE.First then
               if RE.First - RE.Last = 1 then
                  Log ("Merge insert/replacement with earlier insertion");
                  New_Text := RE.Text & New_Text;
                  --  Might cover multiple earlier inserted replacements
                  if TR.Entries.Last = C then
                     Delete (TR.Entries, C);
                  else
                     declare
                        Dummy : Cursor := C;
                     begin
                        Delete (TR.Entries, Dummy);
                     end;
                  end if;
               elsif First - Last = 1 then
                  Log ("Merge insertion with earlier replacement");
                  RE.Text := New_Text & RE.Text;
                  Replace_Element (TR.Entries, C, RE);
                  return True;
               elsif Last < RE.Last then
                  Log ("Replacements at the same offset; keep the earlier");
                  return False;
               elsif RE.Last < Last then
                  Log ("Replacements at the same offset; keep the current");
                  --  Might cover multiple earlier inserted replacements
                  if TR.Entries.Last = C then
                     Delete (TR.Entries, C);
                  else
                     declare
                        Dummy : Cursor := C;
                     begin
                        Delete (TR.Entries, Dummy);
                     end;
                  end if;
               else
                  if RE.Text = New_Text then
                     Log ("Identical replacements");
                     return True;
                     --  TODO: Why return False / True?
                     --  Both the current is accepted and the old is kept
                     --  (since both are identical)
                  else
                     Log
                       ("Conflicting replacements of equal length; " &
                        "keep the current");
                     RE.Text := New_Text;
                     Replace_Element (TR.Entries, C, RE);
                     return True;
                  end if;
               end if;

            elsif First < RE.First then
               if RE.Last = Last then
                  --  length != 0
                  if RE.First - RE.Last = 1 then
                     Log ("Merging of operation with insert operation");
                     RE.First := First;
                     RE.Text  := New_Text & RE.Text;
                     Replace_Element (TR.Entries, C, RE);
                     return True;
                  else
                     Log
                       ("Replacements at the same end-offset; " &
                        "keep the current");
                     RE.First := First;
                     RE.Text  := New_Text;
                     Replace_Element (TR.Entries, C, RE);
                     return True;
                  end if;
               elsif RE.Last < Last then
                  Log
                    ("Earlier replacement is completely covered " &
                     "by the current");
                  --  Might cover multiple earlier inserted replacements
                  if TR.Entries.Last = C then
                     Delete (TR.Entries, C);
                  else
                     declare
                        Dummy : Cursor := C;
                     begin
                        Delete (TR.Entries, Dummy);
                     end;
                  end if;
               elsif RE.First - 1 < Last then
                  Log ("Overlapping edit operations (1)");
                  return False;
               else
                  Log ("Insert current operation before earlier operation");
                  exit;
               end if;

            else
               --  RE.First < First
               if Last = RE.Last then
                  --  entry.length != 0
                  if First - Last = 1 then
                     Log ("Merging of operation with insert operation");
                     RE.Text := RE.Text & New_Text;
                     Replace_Element (TR.Entries, C, RE);
                     return True;
                  else
                     Log
                       ("Replacements at the same end-offset; " &
                        "keep the earlier");
                     return False;
                  end if;
               elsif Last < RE.Last then
                  Log
                    ("Current replacement is completely covered " &
                     "by the earlier");
                  return False;
               elsif First - 1 < RE.Last then
                  Log ("Overlapping edit operations (2)");
                  return False;
               else
                  Next (C);
               end if;
            end if;
         end;
      end loop;

      --  To have "the largest replacement wins" independent of the order
      --  we cannot check for trivial replacements
      --  since by removing the replacement,
      --  a later smaller replacement can unexpectedly win!
      declare
         RE : Replacement_Entry;
      begin
         RE.First := First;
         RE.Last  := Last;
         RE.Text  := New_Text;
         Log ("Add replacement");
         Insert (TR.Entries, C, RE);
      end;

      return True;
   end Replace_Inner;

   --
   --  Debug functionality to check replace
   --

   --  Max_Length_Text : constant Integer := 30;
   --
   --  function Image (TR : Text_Rewrite) return String
   --  is
   --     Return_Value : Unbounded_String;
   --  begin
   --     for E of TR.Entries loop
   --        Return_Value := Return_Value & E.First'Image & "-"
   --                        & E.Last'Image & ":"
   --                        & Head (Image (To_Text (E.Text)), Max_Length_Text)
   --                        & ASCII.CR & ASCII.LF;
   --     end loop;
   --
   --     return To_String (Return_Value);
   --  end Image;
   --
   --  function Is_Internally_Consistent (TR : Text_Rewrite) return Boolean
   --  is
   --     Last : Integer := -1;
   --  begin
   --     for E of TR.Entries loop
   --        if Last > E.First then
   --           return False;
   --        else
   --           Last := E.Last;
   --        end if;
   --     end loop;
   --     return True;
   --  end Is_Internally_Consistent;
   --
   --  function Replace (TR : in out Text_Rewrite;
   --                    First, Last : Natural;
   --                    Text : Wide_Wide_String)
   --              return Boolean
   --  is
   --     Return_Value : Boolean;
   --  begin
   --     Log ("Replace " & First'Image & "-" & Last'Image & ":" &
   --            Head (Image (To_Text (To_Unbounded_Wide_Wide_String (Text))),
   --                  Max_Length_Text));
   --     Return_Value := Replace_Inner (TR, First, Last, Text);
   --     Assert (Check => Is_Internally_Consistent (TR),
   --             Message => "Text Rewrite no longer internally consistent" &
   --               ASCII.CR & ASCII.LF & Image (TR));
   --     return Return_Value;
   --  end Replace;

   function Replace
     (TR : in out Text_Rewrite; First, Last : Natural; Text : Wide_Wide_String)
      return Boolean is
     (Replace_Inner (TR, First, Last, Text));
   --  Proxy to allow easy switching to debug variant

   --  Private: Debugging utilities --------

   procedure Log (Str : String) is
   begin
      if DEBUG then
         Put_Line (Str);
      end if;
   end Log;

end Rejuvenation.Text_Rewrites;
