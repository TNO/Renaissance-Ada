with Rewriters_Minimal_Parentheses; use Rewriters_Minimal_Parentheses;

package Predefined_Rewriters_Minimal_Parentheses is

   RMP : aliased constant Rewriter_Minimal_Parentheses;

private

   RMP : aliased constant Rewriter_Minimal_Parentheses :=
          Make_Rewriter_Minimal_Parentheses;

end Predefined_Rewriters_Minimal_Parentheses;
