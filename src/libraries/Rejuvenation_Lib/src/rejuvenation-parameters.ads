package Rejuvenation.Parameters is

   function Param_Expressions
     (Nm : Libadalang.Analysis.Name) return Param_Actual_Array with
      Pre => not Nm.Is_Null and then Nm.P_Is_Call;
      --  Returns the list of Param Expressions of a function call
      --  including the default expressions for unspecified parameters
      --  including the object name in case of a prefix notation

   function Param_Expressions (Stat : Call_Stmt) return Param_Actual_Array;
   --  function Param_Expressions (Id: Identifier) return Param_Actual_Array;
   --  Returns the list of Param Expressions of a function call
   --  including the default expressions for unspecified parameters

   function Param_Expressions
     (GPI : Generic_Package_Instantiation) return Param_Actual_Array;
   --  Returns the list of Param Expressions of a generic package instantiation
   --  including the default expressions for unspecified parameters

   function Get_Expression
     (The_Array : Param_Actual_Array; Name : String) return Expr'Class;
   --  Return Expression associate with Name in the Param_Actual_Array.
   --  Returns No_Expr when Name is absent in the Param_Actual_Array.

end Rejuvenation.Parameters;
