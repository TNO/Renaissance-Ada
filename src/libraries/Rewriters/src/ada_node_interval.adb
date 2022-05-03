with Ada.Assertions;    use Ada.Assertions;
with Ada.Text_IO;       use Ada.Text_IO;
with Libadalang.Common; use Libadalang.Common;

package body Ada_Node_Interval is

   function Get_Interval_With_Trivia (Node : Ada_Node) return Interval is
   begin
      Assert (not Node.Is_Null, "Node unexpectedly null");
      return
        Make_Interval (Start_Offset (Node, Before), End_Offset (Node, After));
   end Get_Interval_With_Trivia;

end Ada_Node_Interval;
