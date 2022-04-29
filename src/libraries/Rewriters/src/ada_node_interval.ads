with Libadalang.Analysis;         use Libadalang.Analysis;
with Rejuvenation.Node_Locations; use Rejuvenation.Node_Locations;
with Intervals;                   use Intervals;

package Ada_Node_Interval is

   type Get_Interval is access function (Node : Ada_Node) return Interval;

   generic
      Before, After : Node_Location;
   function Get_Interval_With_Trivia (Node : Ada_Node) return Interval;

end Ada_Node_Interval;
