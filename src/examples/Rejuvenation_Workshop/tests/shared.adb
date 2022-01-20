with Libadalang.Common; use Libadalang.Common;

package body Shared is

   function Nr_Of_Parameters
     (S_S : Subp_Spec)
      return Integer
   is
      Return_Value : Integer := 0;
   begin
      for Param of S_S.F_Subp_Params.F_Params loop
         Return_Value := Return_Value + Param.F_Ids.Children'Length;
      end loop;

      return Return_Value;
   end Nr_Of_Parameters;

   function Is_Part_Of_Subp_Def
     (S_S : Subp_Spec)
      return Boolean
   is (S_S.Parent.Kind in
        Ada_Expr_Function           |
          Ada_Generic_Subp_Internal |
          Ada_Null_Subp_Decl        |
          Ada_Subp_Decl             |
          Ada_Subp_Renaming_Decl);

   function Inside_Private_Part
     (Node : Ada_Node'Class)
      return Boolean
   is
     (for some Parent of Node.Parents => Parent.Kind = Ada_Private_Part);

end Shared;
