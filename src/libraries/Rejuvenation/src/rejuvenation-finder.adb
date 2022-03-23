package body Rejuvenation.Finder is

   -- Public: Find Node_Kind --------

   function Find
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Node_List.Vector is
     (Find_Predicate (Node, Predicate, Into));

   function Find_Non_Contained
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Node_List.Vector is
     (Find_Predicate (Node, Predicate, Over));

   function Find
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Node_List.Vector
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
      begin
         return Node.Kind = Node_Kind;
      end Predicate;
   begin
      return Find_Predicate (Node, Predicate'Access, Into);
   end Find;

   function Find_Non_Contained
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Node_List.Vector
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
      begin
         return Node.Kind = Node_Kind;
      end Predicate;
   begin
      return Find_Predicate (Node, Predicate'Access, Over);
   end Find_Non_Contained;

   function Find_First
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type) return Ada_Node
   is
      function Predicate (Node : Ada_Node'Class) return Boolean;
      function Predicate (Node : Ada_Node'Class) return Boolean is
      begin
         return Node.Kind = Node_Kind;
      end Predicate;

      use Node_List;
      Results : constant Node_List.Vector :=
        Find_Predicate (Node, Predicate'Access, Stop);
   begin
      return
        (if Results.Is_Empty then No_Ada_Node else Element (Results.First));
   end Find_First;

   function Find_Sub_List
     (Node : Ada_Node'Class; Node_Kinds : Node_Kind_Type_Array)
      return Node_List_List.Vector
   is
   begin
      return Find_NK_Sub_List (Node, Node_Kinds);
   end Find_Sub_List;

   -- Public: Find Match_Pattern --------

   function Find_Full
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector
   is
   begin
      return Find_MP (Node, Find_Pattern.As_Ada_Node, Into);
   end Find_Full;

   function Find_Non_Contained_Full
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector
   is
   begin
      return Find_MP (Node, Find_Pattern.As_Ada_Node, Over);
   end Find_Non_Contained_Full;

   function Find_First_Full
     (Node   :     Ada_Node'Class; Find_Pattern : Pattern;
      Result : out Match_Pattern) return Boolean
   is
      use Match_Pattern_List;
      Results : constant Match_Pattern_List.Vector :=
        Find_MP (Node, Find_Pattern.As_Ada_Node, Stop);
   begin
      if Results.Is_Empty then
         return False;
      else
         Result := Element (Results.First);
         return True;
      end if;
   end Find_First_Full;

   function Find_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern; Next : Containment)
      return Match_Pattern_List.Vector;
   function Find_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern; Next : Containment)
      return Match_Pattern_List.Vector
   is
      Find_Node : constant Ada_Node := Find_Pattern.As_Ada_Node;
   begin
      if Find_Node.Kind in Ada_Ada_List then
         return Find_MP_Sub_List (Node, Find_Node.Children, Next);
      else
         raise Pattern_Is_No_List_Exception;
      end if;
   end Find_Sub_List;

   function Find_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector is
     (Find_Sub_List (Node, Find_Pattern, Contained));

   function Find_Non_Contained_Sub_List
     (Node : Ada_Node'Class; Find_Pattern : Pattern)
      return Match_Pattern_List.Vector is
     (Find_Sub_List (Node, Find_Pattern, Non_Contained));

   function Find_Full
     (Node : Ada_Node'Class; Find_Patterns : Pattern_Array)
      return Match_Pattern_List.Vector
   is
      Result : Match_Pattern_List.Vector;
   begin
      for Find_Pattern of Find_Patterns loop
         Result.Append (Find_Full (Node, Find_Pattern));
      end loop;
      return Result;
   end Find_Full;

   -- Private --------

   function Find_Predicate
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean;
      Next : Visit_Status) return Node_List.Vector
   is
      Result : Node_List.Vector;

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Predicate (Node) then
            Result.Append (Ada_Node (Node));
            --  TODO: look up which was prefer Node.As_Ada_Node or this
            return Next;
         else
            return Into;
         end if;
      end Visit;

   begin
      Node.Traverse (Visit'Access);
      return Result;
   end Find_Predicate;

   function Find_MP
     (Node : Ada_Node'Class; Pattern : Ada_Node; Next : Visit_Status)
      return Match_Pattern_List.Vector
   is
      Result : Match_Pattern_List.Vector;

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
         MP      : Match_Pattern;
         Success : constant Boolean :=
           MP.Match_Full (Pattern, Ada_Node (Node));
      begin
         if Success then
            Result.Append (MP);
            return Next;
         else
            return Into;
         end if;
      end Visit;

   begin
      Node.Traverse (Visit'Access);
      return Result;
   end Find_MP;

   function Find_NK_Sub_List
     (Node : Ada_Node'Class; Node_Kinds : Node_Kind_Type_Array)
      return Node_List_List.Vector
   is
      Result : Node_List_List.Vector;

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind in Ada_Ada_List then
            for Node_Index in
              Node.Children'First .. (Node.Children'Last - Node_Kinds'Last)
            loop
               declare
                  Success : Boolean;
                  Nodes   : Node_List.Vector;
               begin
                  Success := True;
                  for Kind_Index in Node_Kinds'Range
                  loop -- array range starts at 0
                     if Node.Child (Node_Index + Kind_Index).Kind =
                       Node_Kinds (Kind_Index)
                     then
                        Nodes.Append (Node.Child (Node_Index + Kind_Index));
                     else
                        Success := False;
                     end if;
                  end loop;

                  if Success then
                     Result.Append (Nodes);
                  end if;
               end;
            end loop;
         end if;
         return Into;
      end Visit;

   begin
      Node.Traverse (Visit'Access);
      return Result;
   end Find_NK_Sub_List;

   function Find_MP_Sub_List
     (Node : Ada_Node'Class; Pattern : Ada_Node_Array; Next : Containment)
      return Match_Pattern_List.Vector
      --  Special cases:
      --  We do not allow matches to contain overlapping nodes
      --  E.g. When the pattern $S_Stmt1; $S_Stmt2;
      --  is used to find a sublist in the list of nodes "A; B; C; D;"
      --  We find "A; B;" and "C; D;"
      --  Hence "B; C;" is NOT found
      --
      --  Non-Contained:
      --  Don't go into matches, but when no match go into!

   is
      Result : Match_Pattern_List.Vector;

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind in Ada_Ada_List then
            declare
               Upperbound : constant Integer :=
                 Node.Last_Child_Index - Pattern'Length + 1;
            --  last possible index / start position to fit the whole pattern
            --  into remaining tail of the list
               Skip : Natural := Node.First_Child_Index - 1;
               --  skip counter to prevent overlapping matches
            begin
               for Node_Index in Node.Children'Range loop
                  if Node_Index > Skip and then Node_Index <= Upperbound then
                     declare
                        MP      : Match_Pattern;
                        Success : constant Boolean :=
                          MP.Match_Prefix (Pattern, Node.Children, Node_Index);
                     begin
                        if Success then
                           Result.Append (MP);
                           Skip := Node_Index + Pattern'Length - 1;
                        end if;
                     end;
                  end if;
                  --  Do we need to vist this node?
                  if Next = Contained or else Node_Index > Skip then
                     Node.Child (Node_Index).Traverse (Visit'Access);
                  end if;
               end loop;
               return Over;
            end;
         end if;
         return Into;
      end Visit;

   begin
      Node.Traverse (Visit'Access);
      return Result;
   end Find_MP_Sub_List;

end Rejuvenation.Finder;
