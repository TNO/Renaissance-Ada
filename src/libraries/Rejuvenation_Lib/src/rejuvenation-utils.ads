with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;

package Rejuvenation.Utils is

   -- Raw signature --------

   function Raw_Signature
     (Node   : Ada_Node'Class;
      Before : Node_Location := No_Trivia;
      After  : Node_Location := No_Trivia)
      return String;
   --  Return the original text for the node, using the charset of the
   --  corresponding analysis unit.
   function Raw_Signature
     (Token : Token_Reference; Charset : String) return String;
   --  Return the original text for the token, using the given charset.
   function Raw_Signature
     (First_Node, Last_Node : Ada_Node'Class;
      Before, After : Node_Location := No_Trivia)
      return String;
   --  Return the original text for the first node until the last node,
   --  using the charset of the corresponding analysis unit.

   function Are_Equal_As_Raw_Signature
     (Node1, Node2 : Ada_Node'Class) return Boolean;
   --  Return whether two AST nodes have the same original text.
   function Are_Equal_Case_Insensitive_As_Raw_Signature
     (Node1, Node2 : Ada_Node'Class) return Boolean;
   --  Return whether two AST nodes have the same original text
   --  when ignoring casing.

   --  Package (Distributed over files) functionality

   function In_Same_Package (Unit1, Unit2 : Analysis_Unit) return Boolean;
   --  Two analysis units are in the same package X
   --  when their files names (excluding the extension) are the same.
   --  For example, the analysis units with file names 'X.adb' and 'X.ads'
   --  are in the same package 'X'.

   -- Image --------

   function Image (Node_List_Vector : Node_List.Vector) return String;
   --  Return the Image of each node.

   --  Get tokens --------

   function Get_Trivia_Before
     (Node : Ada_Node'Class) return Token_List.Vector;
   --  Return the trivia (whitespace, comments) tokens
   --  in front of the AST node.
   function Get_Trivia_Before
     (Token : Token_Reference) return Token_List.Vector;
   --  Return the trivia (whitespace, comments) tokens
   --  in front of the token.
   function Get_Trivia_After
     (Node : Ada_Node'Class) return Token_List.Vector;
   --  Return the trivia (whitespace, comments) tokens
   --  after the AST node.
   function Get_Trivia_After
     (Token : Token_Reference) return Token_List.Vector;
   --  Return the trivia (whitespace, comments) tokens
   --  after the token.

   function Get_Tokens (Node : Ada_Node'Class) return Token_List.Vector;
   --  Return the tokens that form the AST node.

end Rejuvenation.Utils;
