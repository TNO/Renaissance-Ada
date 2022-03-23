package body Make_Ada is

   function Make_Call
     (Name                  : String;
      Actual_Parameter_Part : Vector)
      return String
   is
      Actual_Parameter_Part_String : constant String :=
        (if Actual_Parameter_Part.Is_Empty then ""
         else " (" & Join (Actual_Parameter_Part, ", ") & ")");
   begin
      return Name & Actual_Parameter_Part_String;
   end Make_Call;

   function Make_Parameter_Association (Name : String := "";
                                        Value : String := "1")
                                        return String
   is
   begin
      if Name = "" then
         return Value;
      else
         return Name & " => " & Value;
      end if;
   end Make_Parameter_Association;
end Make_Ada;
