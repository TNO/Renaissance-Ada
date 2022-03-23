package Rejuvenation.Printer is

   function Print (Node : Ada_Node'Class) return String;
   --  WARNING: very incomplete pretty-printer!
   --  Note: procedure Print from libadalang-analysis.ads is not a
   --        pretty printer, but prints the AST.

end Rejuvenation.Printer;
