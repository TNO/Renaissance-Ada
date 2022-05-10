with Ada.Containers.Indefinite_Vectors;
with Rewriters; use Rewriters;

package Rewriters_Vectors is new Ada.Containers.Indefinite_Vectors
  (Positive, Rewriter'Class);
