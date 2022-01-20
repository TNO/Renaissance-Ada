with Ada.Text_IO;             use Ada.Text_IO;
with Rejuvenation.Utils;      use Rejuvenation.Utils;

package body Rewriters is

   function Rewrite
     (R    : Rewriter;
      Node : Ada_Node'Class;
      Top_Level : Boolean := True)
      return String
   is
   begin
      Put_Line ("Base function");
      return Raw_Signature (Node);
   end Rewrite;
   --  Since we must provide an implementation,
   --  we have chosen the identity rewriter

end Rewriters;
