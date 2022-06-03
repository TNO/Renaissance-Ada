with Ada.Containers.Indefinite_Vectors;
with Patchers;                    use Patchers;
with Predefined_Rewriters_Append; use Predefined_Rewriters_Append;
with Predefined_Rewriters_Block_Statement_Simplify;
use Predefined_Rewriters_Block_Statement_Simplify;
with Predefined_Rewriters_Boolean_Expression_De_Morgan;
use Predefined_Rewriters_Boolean_Expression_De_Morgan;
with Predefined_Rewriters_Declaration_Simplify;
use Predefined_Rewriters_Declaration_Simplify;
with Predefined_Rewriters_If_Expression_Distribution;
use Predefined_Rewriters_If_Expression_Distribution;
with Predefined_Rewriters_If_Expression_Simplify;
use Predefined_Rewriters_If_Expression_Simplify;
with Predefined_Rewriters_Membership_Test;
use Predefined_Rewriters_Membership_Test;
with Predefined_Rewriters_Minimal_Parentheses;
use Predefined_Rewriters_Minimal_Parentheses;
with Predefined_Rewriters_Not; use Predefined_Rewriters_Not;
with Predefined_Rewriters_Prefer_If_Expression;
use Predefined_Rewriters_Prefer_If_Expression;
with Predefined_Rewriters_Prefer_Quantified_Expressions;
use Predefined_Rewriters_Prefer_Quantified_Expressions;
with Predefined_Rewriters_Representation_Clauses;
use Predefined_Rewriters_Representation_Clauses;
with Post_Processing_Contexts_Function_Access;
use Post_Processing_Contexts_Function_Access;
with Rewriters_Sequence; use Rewriters_Sequence;
with Rewriters_Vectors;  use Rewriters_Vectors;

package Predefined_Patchers is

   Patcher_Append : aliased constant Patcher :=
     Make_Patcher
       ("Append",
        Make_Post_Processing_Context_Function_Access
          (Append_Rewrite_Context'Access),
        Rewriter_Append);

   Patcher_De_Morgan : aliased constant Patcher :=
     Make_Patcher
       ("De_Morgan",
        Make_Post_Processing_Context_Function_Access
          (De_Morgan_Rewrite_Context'Access),
        Rewrite_De_Morgan,
        Make_Rewriter_Sequence (Rewriter_Not & Rewriter_Minimal_Parentheses));

   Patcher_Declarations_Combine : aliased constant Patcher :=
     Make_Patcher
       ("Declarations_Combine",
        Make_Post_Processing_Context_Function_Access
          (Declarations_Combine_Rewrite_Context'Access),
        Rewriter_Declarations_Combine);

   Patcher_Declare_And_Overwrite : aliased constant Patcher :=
     Make_Patcher
       ("Declare_And_Overwrite", Rewriter_Declare_And_Overwrite,
        Make_Rewriter_Sequence
          (Rewriter_If_Expression_Distribution &
           Rewriter_If_Expression_Simplify & Rewriter_Not &
           Rewriter_Minimal_Parentheses));

   Patcher_If_Expression : aliased constant Patcher :=
     Make_Patcher
       ("If_Expression",
        Make_Post_Processing_Context_Function_Access
          (If_Expression_Rewrite_Context'Access),
        Rewriter_If_Expression,
        Make_Rewriter_Sequence
          (Rewriter_If_Expression_Distribution &
           Rewriter_If_Expression_Simplify & Rewriter_Not &
           Rewriter_Minimal_Parentheses));

   Patcher_Membership_Test : aliased constant Patcher :=
     Make_Patcher
       ("Membership_Test",
        Make_Post_Processing_Context_Function_Access
          (Membership_Rewrite_Context'Access),
        Rewriter_Membership_Test);

   Patcher_Quantified_Expressions : aliased constant Patcher :=
     Make_Patcher
       ("Quantified_Expressions",
        Make_Post_Processing_Context_Function_Access
          (Quantified_Expressions_Rewrite_Context'Access),
        Rewriter_Quantified_Expressions,
        Make_Rewriter_Sequence (Rewriter_Not & Rewriter_Minimal_Parentheses));

   Patcher_Representation_Clauses : aliased constant Patcher :=
     Make_Patcher
       ("Representation_Clauses",
        Make_Post_Processing_Context_Function_Access
          (Representation_Clauses_Rewrite_Context'Access),
        Rewriter_Representation_Clauses);

   --------------------------------------------------------------------------
   package Patchers_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Patcher'Class);
   use Patchers_Vectors;

   Patchers_Predefined : constant Patchers_Vectors.Vector :=
     Patcher_Append & Patcher_De_Morgan & Patcher_Declarations_Combine &
     Patcher_Declare_And_Overwrite & Patcher_If_Expression &
     Patcher_Membership_Test & Patcher_Quantified_Expressions &
     Patcher_Representation_Clauses;

end Predefined_Patchers;
