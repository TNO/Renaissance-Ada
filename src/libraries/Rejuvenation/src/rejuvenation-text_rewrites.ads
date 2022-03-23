with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Rejuvenation.Node_Locations;     use Rejuvenation.Node_Locations;

private with Langkit_Support.Text;

package Rejuvenation.Text_Rewrites is

   type Text_Rewrite is tagged private;
   --  The class Text_Rewrite collects multiple text rewrite operations, and
   --  afterwards applies them systematically. It combines two types of syntax
   --  for specifying rewrites:
   --    Abstract syntax: for locations in the original input
   --    Concrete syntax: for any replacing texts
   --  The abstract syntax is focused on physical AST nodes (= abstract syntax)
   --  from Libadalang.

   function Get_Unit (TR : Text_Rewrite) return Analysis_Unit;

   function Is_Initialized (TR : Text_Rewrite) return Boolean;

   function Contains (TR : Text_Rewrite; Node : Ada_Node'Class) return Boolean;
   --  TODO: can this be removed?

   -- Collect rewrite operation (node) --------

   function Remove
     (TR            : in out Text_Rewrite; Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia) return Boolean;
   --  Add the following operation: remove the node from the original text.

   function Prepend
     (TR     : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before :        Node_Location := No_Trivia; Charset : String := "")
      return Boolean;
   --  Add the following operation: insert the given text before the node.

   function Append
     (TR    : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      After :        Node_Location := No_Trivia; Charset : String := "")
      return Boolean;
   --  Add the following operation: insert the given text behind the node.

   function Replace
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before, After :    Node_Location := No_Trivia; Charset : String := "")
      return Boolean;
   --  Add the following operation: replace the node in the original text
   --  by the given text.

   function Restore
     (TR : in out Text_Rewrite; Node : Ada_Node'Class) return Boolean;
--  Add the following operation: restore the original text for the given node.

   procedure Remove
     (TR            : in out Text_Rewrite; Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia);
   --  Add the following operation: remove the original text for the node.

   procedure Prepend
     (TR     : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before :        Node_Location := No_Trivia; Charset : String := "");
   --  Add the following operation: insert the given text
   --  before the original text for the node.

   procedure Append
     (TR    : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      After :        Node_Location := No_Trivia; Charset : String := "");
   --  Add the following operation: insert the given text
   --  behind the original text for the node.

   procedure Replace
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before, After :    Node_Location := No_Trivia; Charset : String := "");
   --  Add the following operation: replace the original text for the node
   --  by the given text.

   procedure Restore (TR : in out Text_Rewrite; Node : Ada_Node'Class);
--  Add the following operation: restore the original text for the given node.

   procedure ReplaceAround
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Before_Text : String;
      Innernode :        Ada_Node'Class; After_Text : String;
      Before : Node_Location := No_Trivia; After : Node_Location := No_Trivia;
      Charset   :        String        := "");
   --  Add the following operation:
   --      replace the node by the given texts around the given innernode.
--  Note that the innernode can still be changed by other rewrite operations.
--  TODO: Given this procedure, do we still need
   --    * the rule 'largest replacement wins' and
   --    * the Restore function?

   -- Collect rewrite operation (nodes) ---------------

   function Remove
     (TR : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia) return Boolean;
   --  Add the following operation:
   --  remove the original text from the first token of the first node
   --  up to (and including) the last token of the last node.

   function Replace
     (TR      : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Text    :        String; Before, After : Node_Location := No_Trivia;
      Charset :        String := "") return Boolean;
   --  Add the following operation:
   --  replace the original text from the first node up to (and including)
   --  the last node, expanded according to the provided expansion mode,
   --  by the given text.

   procedure Remove
     (TR : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia);
   --  Add the following operation: remove the original text
   --  for the consecutive nodes.
   procedure Replace
     (TR      : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Text    :        String; Before, After : Node_Location := No_Trivia;
      Charset :        String := "");
   --  Add the following operation: replace the original text for the nodes
   --  by the given text.

   -- Collect rewrite operation (token) --------

   function Remove
     (TR : in out Text_Rewrite; Token : Token_Reference) return Boolean;
   --  Add the following operation: remove the original text for the token.
   function Prepend
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean;
   --  Add the following operation: insert the given text
   --  before the original text for the token.
   function Append
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean;
   --  Add the following operation: insert the given text
   --  behind the original text for the token.
   function Replace
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "") return Boolean;
   --  Add the following operation: replace the original text for the token
   --  by the given text.

   procedure Remove (TR : in out Text_Rewrite; Token : Token_Reference);
   --  Add the following operation: remove the original text for the token.
   procedure Prepend
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "");
   --  Add the following operation: insert the given text
   --  before the original text for the token.
   procedure Append
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "");
   --  Add the following operation: insert the given text
   --  behind the original text for the token.
   procedure Replace
     (TR      : in out Text_Rewrite; Token : Token_Reference; Text : String;
      Charset :        String := "");
   --  Add the following operation: replace the original text for the token
   --  by the given text.

   -- Inspect rewrite operations --------

   function HasReplacements (TR : Text_Rewrite) return Boolean;
   --  Return whether any rewrite operations have been collected.

   function ApplyToString (TR : Text_Rewrite) return String;
   --  Return the result of applying the collected rewrite operations
   --  to the Text Rewrite.

   function Make_Text_Rewrite_Node
     (Node : Ada_Node'Class; Before, After : Node_Location := No_Trivia)
      return Text_Rewrite;
   --  Text Rewrite for a part (a node)

   function Make_Text_Rewrite_Nodes
     (First_Node, Last_Node : Ada_Node'Class;
      Before, After         : Node_Location := No_Trivia) return Text_Rewrite;
   --  Text Rewrite for a part (a sequence of nodes)

   type Text_Rewrite_Unit is new Text_Rewrite with private;
   --  Text Rewrite for a Unit

   function Make_Text_Rewrite_Unit
     (Unit : Analysis_Unit) return Text_Rewrite_Unit;

   procedure Apply (TR : Text_Rewrite_Unit);
   --  Apply the collected rewrite operations to the file

   procedure Apply_And_Reparse (TR : in out Text_Rewrite_Unit) with
      Post => not TR.HasReplacements;
      --  Apply the collected rewrite operations to the file
      --  The collected rewrite operations are removed and the Unit is reparsed

private

   -- Internal data structures -------

   type Replacement_Entry is record
      First, Last : Natural;
      Text        : Unbounded_Wide_Wide_String;
   end record;
--  Each Replacement_Entry indicates a replacement: in the current contents
--  the substring from index First up to and including index Last is replaced
--  by Text (possibly with a different length).

   package Replacement_List is new Vectors (Positive, Replacement_Entry);

   type Text_Rewrite is tagged record
      Unit : Analysis_Unit := No_Analysis_Unit;
      --  Unit on which replacement operations will be performed
      First, Last : Positive;
      --  First and Last position of original text in Unit text
      Entries : Replacement_List.Vector;
      --  List of collected replacement operations - in Unit positions
   end record;

   -- Collect rewrite operation --------

   function Replace
     (TR : in out Text_Rewrite; First, Last : Natural; Text : Wide_Wide_String)
      return Boolean with
      Pre => Last + 1 >= First;
      --  Add the following operation: replace the original text
      --  from index First up to and including index Last by the given text.

      -- Function Expressions --

   function Remove
     (TR            : in out Text_Rewrite; Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia) return Boolean is
     (Replace (TR, Node, "", Before, After));

   function Replace
     (TR : in out Text_Rewrite; Node : Ada_Node'Class; Text : String;
      Before, After :    Node_Location := No_Trivia; Charset : String := "")
      return Boolean is
     (Replace (TR, Node, Node, Text, Before, After, Charset));

   function Restore
     (TR : in out Text_Rewrite; Node : Ada_Node'Class) return Boolean is
     (Replace
        (TR, Node, Langkit_Support.Text.Image (Node.Text), Before => No_Trivia,
         After => No_Trivia));

   function Remove
     (TR : in out Text_Rewrite; First_Node, Last_Node : Ada_Node'Class;
      Before, After :        Node_Location := No_Trivia) return Boolean is
     (Replace (TR, First_Node, Last_Node, "", Before, After));

   function Remove
     (TR : in out Text_Rewrite; Token : Token_Reference) return Boolean is
     (Replace (TR, Token, ""));

   function Get_Unit (TR : Text_Rewrite) return Analysis_Unit is (TR.Unit);

   function Is_Initialized (TR : Text_Rewrite) return Boolean is
     ("/=" (TR.Unit, No_Analysis_Unit));

   function Contains
     (TR : Text_Rewrite; Node : Ada_Node'Class) return Boolean is
     (TR.First <= Start_Offset (Node) and then End_Offset (Node) <= TR.Last);

   function Make_Text_Rewrite_Node
     (Node  : Ada_Node'Class; Before : Node_Location := No_Trivia;
      After : Node_Location := No_Trivia) return Text_Rewrite is
     (Make_Text_Rewrite_Nodes (Node, Node, Before, After));

   --  Text Rewrite for a Unit
   type Text_Rewrite_Unit is new Text_Rewrite with null record;

   -- Debugging utilities --------

   DEBUG : Boolean := False;
   --  Flag that indicates whether internal debugging information
   --  is displayed on the console.

   procedure Log (Str : String);

end Rejuvenation.Text_Rewrites;
