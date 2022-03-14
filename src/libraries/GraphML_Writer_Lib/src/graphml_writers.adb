pragma Assertion_Policy (Check);

with Ada.Strings.Fixed;

package body GraphML_Writers is

   package S_F renames Ada.Strings.Fixed;

   use type S_U.Unbounded_String;

   function "+" (Str : String) return S_U.Unbounded_String is
     (S_U.To_Unbounded_String (Str));

   NODE_TAG : constant String := "node";
   EDGE_TAG : constant String := "edge";

   NAME_TAG    : constant S_U.Unbounded_String := +"name";
   TYPE_TAG    : constant S_U.Unbounded_String := +"type";
   SUBTYPE_TAG : constant S_U.Unbounded_String := +"subtype";

   function Edge_Data_Hash (Id : Edge_Data) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return
        5 * Ada.Containers.Hash_Type (Id.Source_Id) +
        3 * Ada.Containers.Hash_Type (Id.Target_Id) + S_U.Hash (Id.Edge_Ty);
   end Edge_Data_Hash;

   function Replace_All
     (Str : String; From : Character; To : String) return String;
   function Replace_All
     (Str : String; From : Character; To : String) return String
   is

      function Replace_Inner (Str : String; Index : Natural) return String;
      function Replace_Inner (Str : String; Index : Natural) return String is
      begin
         if Index = Str'First - 1 then
            return Str;
         elsif Str (Index) = From then
            return
              Replace_Inner
                (S_F.Replace_Slice (Str, Index, Index, To), Index - Str'First);
         else
            return Replace_Inner (Str, Index - 1);
         end if;
      end Replace_Inner;

   begin
      return Replace_Inner (Str, Str'Last);
   end Replace_All;

   function Escape (Str : String) return String;
   function Escape (Str : String) return String is
   begin
      return
        Replace_All
          (Replace_All
             (Replace_All
                (Replace_All (Replace_All (Str, '&', "&amp;"), '<', "&lt;"),
                 '>', "&gt;"),
              '"', "&quot;"),
           ''', "&apos;");
   end Escape;

   function Escape (Str : S_U.Unbounded_String) return String;
   function Escape (Str : S_U.Unbounded_String) return String is
   begin
      return Escape (S_U.To_String (Str));
   end Escape;

   function Image (Ty : GraphML_Type) return String;
   function Image (Ty : GraphML_Type) return String is
   begin
      case Ty is
         when GraphML_Boolean =>
            return "boolean";
         when GraphML_Int =>
            return "int";
         when GraphML_String =>
            return "string";
      end case;
   end Image;

   procedure Write_String
     (Stream : Ada.Streams.Stream_IO.Stream_Access; Indent : Natural;
      Str    : String);
   procedure Write_String
     (Stream : Ada.Streams.Stream_IO.Stream_Access; Indent : Natural;
      Str    : String)
   is
      use S_F;
   begin
      String'Write (Stream, Indent * "  ");
      String'Write (Stream, Str);
      Character'Write (Stream, ASCII.CR);
      Character'Write (Stream, ASCII.LF);
   end Write_String;

   procedure Write_Attribute_Keys
     (Stream     : Ada.Streams.Stream_IO.Stream_Access; For_Tag : String;
      Attributes : Attribute_Definition_Sets.Map);
   procedure Write_Attribute_Keys
     (Stream     : Ada.Streams.Stream_IO.Stream_Access; For_Tag : String;
      Attributes : Attribute_Definition_Sets.Map)
   is
      use Attribute_Definition_Sets;
   begin
      for Attribute in Attributes.Iterate loop
         declare
            Id_Attr : constant String :=
              "id=""" & Escape (Key (Attribute)) & """";
            For_Attr       : constant String := "for=""" & For_Tag & """";
            Attr_Name_Attr : constant String :=
              "attr.name=""" & Escape (Key (Attribute)) & """";
            Attr_Type_Attr : constant String :=
              "attr.type=""" & Image (Element (Attribute)) & """";
         begin
            Write_String
              (Stream, 1,
               "<key " & Id_Attr & " " & For_Attr & " " & Attr_Name_Attr &
               " " & Attr_Type_Attr & " />");
         end;
      end loop;
   end Write_Attribute_Keys;

   function Create_GraphML_Writer
     (Filename        : String;
      Node_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map;
      Edge_Attributes : Attribute_Definition_Sets.Map :=
        Attribute_Definition_Sets.Empty_Map)
      return GraphML_File
   is
      XMLNS : constant String :=
        "xmlns=""http://graphml.graphdrawing.org/xmlns""";
      XMLNS_XSI : constant String :=
        "xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""";
      SCHEMALOCATION : constant String :=
        "schemaLocation=""http://graphml.graphdrawing.org/xmlns "
            & "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd""";
   begin
      return File : GraphML_File do
         Ada.Streams.Stream_IO.Create
           (File.File,
            Mode => Ada.Streams.Stream_IO.Out_File,
            Name => Filename);
         File.Stream := Ada.Streams.Stream_IO.Stream (File.File);

         File.Node_Attributes := Node_Attributes;
         File.Node_Attributes.Insert (NAME_TAG, GraphML_String);
         File.Node_Attributes.Insert (TYPE_TAG, GraphML_String);
         File.Node_Attributes.Insert (SUBTYPE_TAG, GraphML_String);
         File.Edge_Attributes := Edge_Attributes;
         File.Edge_Attributes.Insert (TYPE_TAG, GraphML_String);

         Write_String
           (File.Stream, 0, "<?xml version=""1.0"" encoding=""utf-8""?>");
         Write_String
           (File.Stream, 0,
            "<graphml " & XMLNS & " " & XMLNS_XSI & " " & SCHEMALOCATION &
            " >");
         Write_Attribute_Keys (File.Stream, NODE_TAG, File.Node_Attributes);
         Write_Attribute_Keys (File.Stream, EDGE_TAG, File.Edge_Attributes);
         Write_String (File.Stream, 1, "<graph edgedefault=""directed"">");
      end return;
   end Create_GraphML_Writer;

   procedure Close (This : in out GraphML_File) is
   begin
      Write_String (This.Stream, 1, "</graph>");
      Write_String (This.Stream, 0, "</graphml>");
      Ada.Streams.Stream_IO.Close (This.File);
   end Close;

   procedure Write_Data
     (Stream                : Ada.Streams.Stream_IO.Stream_Access;
      Attribute_Definitions : Attribute_Definition_Sets.Map;
      Attribute_Values      : Attribute_Value_Sets.Map);
   procedure Write_Data
     (Stream                : Ada.Streams.Stream_IO.Stream_Access;
      Attribute_Definitions : Attribute_Definition_Sets.Map;
      Attribute_Values      : Attribute_Value_Sets.Map)
   is
      use Attribute_Value_Sets;
   begin
      for Attribute in Attribute_Values.Iterate loop
         if Attribute_Definitions.Contains (Key (Attribute)) then
            declare
               Key_Attr : constant String :=
                 "key=""" & Escape (Key (Attribute)) & """";
               Value : constant String := Escape (Element (Attribute));
            begin
               Write_String
                 (Stream, 3, "<data " & Key_Attr & ">" & Value & "</data>");
            end;
         end if;
      end loop;
   end Write_Data;

   procedure Write_Node_Internal
     (File : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         : String; Node_Subty : String; Has_Node_Subty : Boolean;
      Node_Attributes :        Attribute_Value_Sets.Map);
   procedure Write_Node_Internal
     (File : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         : String; Node_Subty : String; Has_Node_Subty : Boolean;
      Node_Attributes :        Attribute_Value_Sets.Map)
   is
      Key : constant S_U.Unbounded_String := +Node_Key;
      Id  : constant Node_Id             :=
        (if File.Known_Nodes.Contains (Key) then
           File.Known_Nodes.Element (Key).Id
         else File.Next_Node_Id);
      Data : constant Node_Data :=
        (Id         => Id, Ty => +Node_Ty, Subty => +Node_Subty,
         Attributes => Node_Attributes);
      Id_Attr : constant String :=
        "id=""" & S_F.Trim (Id'Image, Ada.Strings.Left) & """";
      Subty : constant String :=
        (if Has_Node_Subty then ":" & Escape (Node_Subty) else "");
      Labels_Attr : constant String :=
        "labels="":" & Escape (Node_Ty) & Subty & """";
   begin
      if File.Known_Nodes.Contains (Key) then
         return;
      end if;

      File.Known_Nodes.Insert (Key, Data);
      File.Next_Node_Id := Id + 1;

      Write_String
        (File.Stream, 2,
         "<" & NODE_TAG & " " & Id_Attr & " " & Labels_Attr & ">");

      declare
         Attributes : Attribute_Value_Sets.Map := Node_Attributes;
      begin
         Attributes.Insert (NAME_TAG, +Node_Name);
         Attributes.Insert (TYPE_TAG, +Node_Ty);

         if Has_Node_Subty then
            Attributes.Insert (SUBTYPE_TAG, +Node_Subty);
         end if;

         Write_Data (File.Stream, File.Node_Attributes, Attributes);
      end;

      Write_String (File.Stream, 2, "</" & NODE_TAG & ">");
   end Write_Node_Internal;

   procedure Write_Node
     (This : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map)
   is
   begin
      Write_Node_Internal
        (This, Node_Key, Node_Name, String (Node_Ty), "", False,
         Node_Attributes);
   end Write_Node;

   procedure Write_Node
     (This : in out GraphML_File; Node_Key : String; Node_Name : String;
      Node_Ty         :        Node_Type; Node_Subty : Node_Subtype;
      Node_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map)
   is
      NODE_SUBTY_STRING : constant String  := String (Node_Subty);
      HAS_NODE_SUBTY    : constant Boolean := NODE_SUBTY_STRING /= "";
   begin
      Write_Node_Internal
        (This, Node_Key, Node_Name, String (Node_Ty), NODE_SUBTY_STRING,
         HAS_NODE_SUBTY, Node_Attributes);
   end Write_Node;

   procedure Write_Edge_Internal
     (File            : in out GraphML_File; Source_Node_Key : String;
      Target_Node_Key :        String; Edge_Ty : String;
      Edge_Attributes :        Attribute_Value_Sets.Map);
   procedure Write_Edge_Internal
     (File            : in out GraphML_File; Source_Node_Key : String;
      Target_Node_Key :        String; Edge_Ty : String;
      Edge_Attributes :        Attribute_Value_Sets.Map)
   is
      Source_Id : constant Node_Id :=
        File.Known_Nodes.Element (+Source_Node_Key).Id;
      Target_Id : constant Node_Id :=
        File.Known_Nodes.Element (+Target_Node_Key).Id;
      Element    : constant Edge_Data           :=
        (Source_Id  => Source_Id, Target_Id => Target_Id, Edge_Ty => +Edge_Ty,
         Attributes => Edge_Attributes);
      Source_Attr : constant String :=
        "source=""" & S_F.Trim (Source_Id'Image, Ada.Strings.Left) & """";
      Target_Attr : constant String :=
        "target=""" & S_F.Trim (Target_Id'Image, Ada.Strings.Left) & """";
      Label_Attr : constant String := "label=""" & Escape (Edge_Ty) & """";
   begin
      if File.Known_Edges.Contains (Element) then
         return;
      end if;

      File.Known_Edges.Insert (Element);

      Write_String
        (File.Stream, 2,
         "<" & EDGE_TAG & " " & Source_Attr & " " & Target_Attr & " " &
         Label_Attr & ">");

      declare
         TY         : constant String := "type=" & Escape (Edge_Ty);
         SOURCE     : constant String := "source=" & Escape (Source_Node_Key);
         TARGET     : constant String := "target=" & Escape (Target_Node_Key);
         Attributes : Attribute_Value_Sets.Map := Edge_Attributes;
      begin
         Write_String
           (File.Stream, 3,
            "<desc>" & TY & " " & SOURCE & " " & TARGET & " " & TY &
            "</desc>");

         Attributes.Insert (TYPE_TAG, +Edge_Ty);
         Write_Data (File.Stream, File.Edge_Attributes, Attributes);
      end;

      Write_String (File.Stream, 2, "</" & EDGE_TAG & ">");
   end Write_Edge_Internal;

   procedure Write_Edge
     (This            : in out GraphML_File; Source_Node_Key : String;
      Target_Node_Key :        String; Edge_Ty : Edge_Type;
      Edge_Attributes :        Attribute_Value_Sets.Map :=
        Attribute_Value_Sets.Empty_Map)
   is
   begin
      pragma Assert
        (This.Known_Nodes.Contains (+Source_Node_Key),
         "Source_Node for " & String (Edge_Ty) & " absent: " &
         Source_Node_Key);
      pragma Assert
        (This.Known_Nodes.Contains (+Target_Node_Key),
         "Target_Node for " & String (Edge_Ty) & " absent: " &
         Target_Node_Key);
      Write_Edge_Internal
        (This, Source_Node_Key, Target_Node_Key, String (Edge_Ty),
         Edge_Attributes);
   end Write_Edge;

end GraphML_Writers;
