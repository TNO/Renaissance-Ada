package Rejuvenation.Nested is

   function Remove_Nested_Flags (Source : String;
                                 On_Flag : String;
                                 Off_Flag : String;
                                 Depth    : Natural := 0
                                )
                                 return String
     with
       Pre => On_Flag'Length > 0
       and then Off_Flag'Length > 0
       and then On_Flag /= Off_Flag;
   --  Remove Nested Flags
   --  An error is raised when Source doesn't end at Depth = 0
   --  E.g. In case of pretty printing, the default mode is ON,
   --  so the initial depth is one instead of zero.

end Rejuvenation.Nested;
