with Ada.Containers.Vectors;
with Rewriters; use Rewriters;

package Rewriters_Sequence is new Ada.Containers.Vectors
  (Positive, Any_Constant_Rewriter);
