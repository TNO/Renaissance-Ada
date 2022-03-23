with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Vectors;
with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Common;         use Libadalang.Common;

package Rejuvenation is

   type Node_Kind_Type_Array is array (Natural range <>) of Ada_Node_Kind_Type;
   --  Data type for array of node kind types.

   package Node_List is new Vectors (Positive, Ada_Node);
   --  Data type for list of nodes.

   package Node_List_List is new Vectors (Positive,
                                          Node_List.Vector,
                                          "=" => Node_List."=");
   --  Data type for list of list of nodes.

   package Token_List is new Vectors (Positive, Token_Reference);
   --  Data type for list of tokens.

end Rejuvenation;
