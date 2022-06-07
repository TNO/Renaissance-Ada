with Libadalang.Analysis;           use Libadalang.Analysis;
with Rejuvenation;                  use Rejuvenation;
with Rewriters;                     use Rewriters;

--  TODO: add functionality to rejuvenation library
--  to recognize user-specific markers that are never part of an AST node.
--  With this functionality, we can lift the responsibility from the user to
--  not overwrite, remove, change marks.
package Mark_Utils is

   function Add_Marks_And_Pretty_Print_Sections
     (Unit : in out Analysis_Unit; Nodes : Node_List.Vector)
      return Boolean
     with
       Pre => (for all Node of Nodes => Node.Unit = Unit);
   --  Add (line based) Pretty Print Sections to Nodes
   --  Add Mark to Nodes
   --  Rewriters will only make changes to marked nodes (and their children).
   --  Note the file associated with the Unit of the Nodes will be changed.

   procedure Remove_Marks (Filename : String);
   --  Remove Marks from Filename
   --  Note the file will be changed (if marks are present).

   function Is_Marked (Node : Ada_Node'Class) return Boolean;
   --  Is Node Marked?

   function Is_Some_Parent_Marked
     (Node : Ada_Node; With_Self : Boolean := True)
     return Boolean;
   --  Is some parent marked?
   --  With_Self determines whether marking of this node is included

   function Make_Rewriter_Mark_Aware
     (R : Rewriter'Class) return Rewriter'Class;

end Mark_Utils;
