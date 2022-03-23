with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;

package body Generators is

   function Generate_Pattern (G : Generator) return Analysis_Unit is
      Str : constant String :=
        G.SG (String_Vectors.To_Vector (To_String (G.Name), 1));
   begin
      return Analyze_Fragment (Str, G.Rule);
   end Generate_Pattern;

   function Generate_Instance (G : Generator) return Analysis_Unit is
      Str : constant String := G.SG (G.Values);
   begin
      return Analyze_Fragment (Str, G.Rule);
   end Generate_Instance;

end Generators;
