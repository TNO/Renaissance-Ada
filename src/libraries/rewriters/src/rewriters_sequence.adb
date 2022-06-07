package body Rewriters_Sequence is

   overriding function Rewrite
     (R_S : Rewriter_Sequence; Unit : in out Analysis_Unit)
     return Boolean
     --  All rewrites in the sequence must be executed
     --  When any rewrite in the sequence changes the unit,
     --  this rewrite changes the unit.
   is
      Return_Value : Boolean := False;
   begin
      for R of R_S.F_Vector loop
         declare
            Changed : constant Boolean := R.Rewrite (Unit);
         begin
            Return_Value := Return_Value or else Changed;
         end;
      end loop;
      return Return_Value;
   end Rewrite;

end Rewriters_Sequence;
