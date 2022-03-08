with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Streams.Stream_IO;

package GraphML_Writers is

   package SU renames Ada.Strings.Unbounded;

   type GraphML_Type is (GraphML_Boolean, GraphML_Int, GraphML_String);

   package Attribute_Definition_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => SU.Unbounded_String, Element_Type => GraphML_Type,
      Hash     => SU.Hash, Equivalent_Keys => SU."=");

   package Attribute_Value_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => SU.Unbounded_String, Element_Type => SU.Unbounded_String,
      Hash     => SU.Hash, Equivalent_Keys => SU."=", "=" => SU."=");

   type GraphML_File is tagged limited private;

   function Create_GraphML_Writer
     (Filename        : String;
      Node_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map;
      Edge_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map)
      return GraphML_File;

   procedure Close (File : in out GraphML_File);

   type Node_Type is new String;
   type Node_Subtype is new String;

   procedure Write_Node
     (File : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

   procedure Write_Node
     (File : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type; Node_Subty : Node_Subtype;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

   type Edge_Type is new String;

   procedure Write_Edge
     (File            : in out GraphML_File; Source_Node_Key : String;
      Target_Node_Key :        String; Edge_Ty : Edge_Type;
      Edge_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map);

private

   type Node_Id is new Positive;

   type Node_Data is record
      Id         : Node_Id;
      Ty         : SU.Unbounded_String;
      Subty      : SU.Unbounded_String;
      Attributes : Attribute_Value_Sets.Map;
   end record;

   package Known_Node_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type => SU.Unbounded_String, Element_Type => Node_Data,
      Hash     => SU.Hash, Equivalent_Keys => SU."=");

   type Edge_Data is record
      Source_Id  : Node_Id;
      Target_Id  : Node_Id;
      Edge_Ty    : SU.Unbounded_String;
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
