with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;

package Rejuvenation.Match_Patterns is

   type Match_Pattern is tagged private;
   --  The class Match_Pattern represents a single occurrence of an AST pattern
   --  in an AST instance. An AST pattern or AST instance is a list of physical
   --  AST nodes from Libadalang.
   --
   --  AST patterns are expressed as code snippets that can contain
   --  placeholders that can be mapped to AST nodes from the AST instance.
   --  If a placeholder occurs multiple times, then the string values of the
   --  mapped AST nodes must be identical.
   --
   --  To show diagnosis information about non-matches on the console, use:
   --    Rejuvenation.Match_Pattern.DIAGNOSE := True;

   -- Externally-visible data structures -------

   Inconsistent_Placeholder_Values_Exception : exception;
   --  Exception that indicates that a placeholder is assigned
   --  two different values.
   Unsupported_Placeholder_Exception : exception;
   --  Exception that indicates that an unsupported placeholder was observed.
   Invalid_Multiple_Placeholder_Status_Exception : exception;
   --  Internal programming error; this should not happen.

   DIAGNOSE : Boolean := False;
   --  Flag that indicates whether diagnosis information about non-matches is
   --  displayed on the console.

   -- Create match --------
   --  TODO: should order of parameters be in line with similar functions
   --        in Finder interface?
   function Match_Full
     (MP : out Match_Pattern; Pattern : Ada_Node; Instance : Ada_Node)
      return Boolean;
   --  Return whether the single-node AST pattern fully matches
   --  the single-node AST instance.
   --  If succesful, then the match attributes can afterwards be inspected.
   function Match_Full
     (MP       : out Match_Pattern; Pattern : Ada_Node_Array;
      Instance :     Ada_Node_Array) return Boolean;
   --  Return whether the node-array AST pattern fully matches
   --  the node-array AST instance.
   --  If succesful, then the match attributes can afterwards be inspected.
   function Match_Prefix
     (MP       : out Match_Pattern; Pattern : Ada_Node_Array;
      Instance :     Ada_Node_Array; Instance_Start_Index : Integer)
      return Boolean with
      Pre => Instance_Start_Index in Instance'Range;
      --  Return whether the node-array AST pattern matches
      --  a prefix of the node-array AST instance.
      --  If succesful, then the match attributes can afterwards be inspected.

   function Are_Identical
     (Node1 : Ada_Node'Class; Node2 : Ada_Node'Class) return Boolean;
   --  Return whether the ASTs of the two nodes match.
   --  Raises an exception if a placeholder pattern occurs.

   -- Inspect match --------

   function Get_Nodes (MP : Match_Pattern) return Node_List.Vector;
   --  Return the AST instance nodes that match with the AST pattern.

   --  TODO: Should we add an Is_Valid_Placeholder_Name function?

   function Has_Single
     (MP : Match_Pattern; Placeholder_Name : String) return Boolean;
   --  Return whether the "single" placeholder name is mapped
   --  to any AST node from the AST instance.
   function Get_Single_As_Node
     (MP : Match_Pattern; Placeholder_Name : String) return Ada_Node with
      Pre => Has_Single (MP, Placeholder_Name);
      --  Return the mapped AST node from the AST instance
      --  for the given "single" placeholder name.
   function Get_Single_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String with
      Pre => Has_Single (MP, Placeholder_Name);
      --  Return the mapped raw signature from the AST instance
      --  for the given "single" placeholder name.

   function Has_Multiple
     (MP : Match_Pattern; Placeholder_Name : String) return Boolean;
   --  Return whether the "multiple" placeholder name is mapped
   --  to any AST node from the AST instance.
   function Get_Multiple_As_Nodes
     (MP : Match_Pattern; Placeholder_Name : String)
      return Node_List.Vector with
      Pre => Has_Multiple (MP, Placeholder_Name);
      --  Return the mapped AST nodes from the AST instance
      --  for the given "multiple" placeholder name.
   function Get_Multiple_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String with
      Pre => Has_Multiple (MP, Placeholder_Name);
   --  Return the mapped raw signature from the AST instance
   --  for the given "multiple" placeholder name.
   --  Note this includes the separators and trivia (white spaces and comments)
   --  between the multiple nodes.

   function Get_Placeholder_As_Nodes
     (MP : Match_Pattern; Placeholder_Name : String) return Node_List.Vector;
   --  Return the mapped AST nodes from the AST instance
   --  for the given placeholder name (both single and multiple).
   function Get_Placeholder_As_Raw_Signature
     (MP : Match_Pattern; Placeholder_Name : String) return String;
   --  Return the mapped raw signature from the AST instance
   --  for the given placeholder name (both single and multiple).

private

   -- Create match --------

   function Has_Nested_Match_Full
     (MP : Match_Pattern; Pattern : Ada_Node; Instance : Ada_Node)
      return Boolean;
   function Match
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean;
   function Match
     (MP : in out Match_Pattern; Pattern : Ada_Node_Array;
      Instance :        Ada_Node_Array; Instance_Start_Index : Integer;
      Pattern_Must_Cover_End_Of_Instance :    Boolean; Store_Nodes : Boolean)
      return Boolean with
      Pre => Instance'Last < Instance'First
      or else Instance_Start_Index in Instance'Range;
   function Match_Multiple_Placeholder
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean;
   function Match_Single_Placeholder
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean;
   function Match_Specific
     (MP       : in out Match_Pattern; Pattern : Ada_Node'Class;
      Instance :        Ada_Node'Class) return Boolean;

   procedure Dump_Partial_Match (MP : Match_Pattern);

   -- Internal data structures -------

   function Equivalent_Key (Left, Right : String) return Boolean;
   function Equivalent_Element (Left, Right : Ada_Node) return Boolean;
   function Equivalent_Element (Left, Right : Node_List.Vector) return Boolean;

   package Mapping_Single_Map is new Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Ada_Node, Hash => Ada.Strings.Hash,
      Equivalent_Keys => Equivalent_Key, "=" => Equivalent_Element);

   package Mapping_Multiple_Map is new Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Node_List.Vector,
      Hash     => Ada.Strings.Hash, Equivalent_Keys => Equivalent_Key,
      "="      => Equivalent_Element);

   type Match_Pattern is tagged record
      Nodes : Node_List.Vector;
      --  The AST instance nodes that match with the AST pattern.
      Mapping_Single : Mapping_Single_Map.Map;
      --  Mapping from "single" placeholder name to AST node.
      Mapping_Multiple : Mapping_Multiple_Map.Map;
      --  Mapping from "multiple" placeholder name to AST nodes.
   end record;

   type Multiple_Placeholder_Status is record
      Ongoing_Multiple          : Boolean  := False;
      Multiple_PlaceHolder_Name : Ada_Node := No_Ada_Node;
      --  TODO: Why called name and not node?
      Multiple_Placeholder_Nodes : Node_List.Vector;

      Has_Earlier_Multiple_Placeholder_Nodes : Boolean := False;
      Earlier_Multiple_Placeholder_Nodes     : Node_List.Vector;
   end record;

   function Is_Open (MPS : Multiple_Placeholder_Status) return Boolean;
   procedure Open
     (MPS : in out Multiple_Placeholder_Status; MP : Match_Pattern;
      Placeholder_Node :        Ada_Node);
   procedure Close
     (MPS : in out Multiple_Placeholder_Status; MP : in out Match_Pattern);
   procedure Update
     (MPS : in out Multiple_Placeholder_Status; Instance_Node : Ada_Node);

end Rejuvenation.Match_Patterns;
