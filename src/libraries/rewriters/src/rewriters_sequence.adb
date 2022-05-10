package body Rewriters_Sequence is

   overriding procedure Rewrite
     (R_S : Rewriter_Sequence; Unit : in out Analysis_Unit)
   is
   begin
      for R of R_S.F_Vector loop
         R.Rewrite (Unit);
      end loop;
   end Rewrite;

end Rewriters_Sequence;
