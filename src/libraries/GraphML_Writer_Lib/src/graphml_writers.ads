with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Streams.Stream_IO;

package GraphML_Writers is

   package S_U renames Ada.Strings.Unbounded;

   type GraphML_Type is (GraphML_Boolean, GraphML_Int, GraphML_String);

   package Attribute_Definition_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => S_U.Unbounded_String, Element_Type => GraphML_Type,
      Hash     => S_U.Hash, Equivalent_Keys => S_U."=");

   package Attribute_Value_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => S_U.Unbounded_String, Element_Type => S_U.Unbounded_String,
      Hash     => S_U.Hash, Equivalent_Keys => S_U."=", "=" => S_U."=");

   type Node_Type is new String;
   type Node_Subtype is new String;
   type Edge_Type is new String;

   type GraphML_File is tagged limited private;

   procedure Close (This : in out GraphML_File);

   procedure Write_Node
     (This : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

   procedure Write_Node
     (This : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type; Node_Subty : Node_Subtype;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

   procedure Write_Edge
     (This            : in out GraphML_File; Source_Node_Key : String;
      Target_Node_Key :        String; Edge_Ty : Edge_Type;
      Edge_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

   function Create_GraphML_Writer
     (Filename        : String;
      Node_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map;
      Edge_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map)
      return GraphML_File;

private

   type Node_Id is new Positive;

   type Node_Data is record
      Id         : Node_Id;
      Ty         : S_U.Unbounded_String;
      Subty      : S_U.Unbounded_String;
      Attributes : Attribute_Value_Sets.Map;
   end record;

   package Known_Node_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => S_U.Unbounded_String, Element_Type => Node_Data,
      Hash     => S_U.Hash, Equivalent_Keys => S_U."=");

   type Edge_Data is record
      Source_Id  : Node_Id;
      Target_Id  : Node_Id;
      Edge_Ty    : S_U.Unbounded_String;
      Attributes : Attribute_Value_Sets.Map;
   end record;

   function Edge_Data_Hash (Id : Edge_Data) return Ada.Containers.Hash_Type;

   package Known_Edge_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Edge_Data, Hash => Edge_Data_Hash,
      Equivalent_Elements => "=");

   type GraphML_File is tagged limited record
      File            : Ada.Streams.Stream_IO.File_Type;
      Stream          : Ada.Streams.Stream_IO.Stream_Access;
      Node_Attributes : Attribute_Definition_Sets.Map;
      Edge_Attributes : Attribute_Definition_Sets.Map;
      Next_Node_Id    : Node_Id := 1;
      Known_Nodes     : Known_Node_Sets.Map;
      Known_Edges     : Known_Edge_Sets.Set;
   end record;

end GraphML_Writers;
