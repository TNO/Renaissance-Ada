with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Intervals;                   use Intervals;

--  TODO: decide on how to handle comment
--  alternatives
--  * Make place holders for comment (in find and replace patterns)
--    such that the user can specify how to handle the comments in a pattern
--  * Keep current before, after node locations
--    resulting in possible duplication of trivia
--  * Introduce Get_Interval function to allow customer specific assignments
--    of trivia to AST nodes. E.g.in this code base, documentation of
--    functions appear before them, and documentation of statements after them.
--
--  How does this interact with the desire of marks?
--  How to prevent mark (comment) to be copied, included, changed, or removed?
package Ada_Node_Interval is

   type Get_Interval is access function (Node : Ada_Node) return Interval;

   generic
      Before, After : Node_Location;
   function Get_Interval_With_Trivia (Node : Ada_Node) return Interval;
   --  Relate AST Node to text interval by including Trivia
   --  as specified by the `Before` and `After` parameters.
   --  Note: This function doesn't guarantee that
   --  all trivia are uniquely assigned to an AST node.
   --  For example, when `Before` and `After` are both equal to `All_Trivia` the
   --  comment in "A; -- comment\n B;" will be assigned to both "A;" and "B;"
   --  For example, when `Before` and `After` are both equal to `No_Trivia` the
   --  comment in "A; -- comment\n B;" will not be assigned to any AST node.

--   function Get_Interval_Kind_Based (Node : Ada_Node) return Interval;
   --  Relate AST Node to text interval.
   --  When the AST node is a ghost node, an empty interval is returned.
   --  In other words, no trivia are included.
   --  When an AST node directly follows another AST node
   --  (hence separating token is absent) only
   --  all trivia after the AST node are included in the interval.
   --  In all other case, all trivia before and after the AST node are included
   --  in the interval.
   --  Note: This function guarantees that
   --  trivia is not assigned to multiple AST nodes.
   --  This function doesn't guarantees that trivia is assigned to an AST node.

end Ada_Node_Interval;
