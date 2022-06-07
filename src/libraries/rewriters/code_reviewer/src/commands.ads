package Commands is

   type OS_Kind is (WINDOWS, LINUX);
   Operating_System : constant OS_Kind := WINDOWS;
   --  Set platform correctly before running Code_Reviewer

   No_Output : constant String :=
     (case Operating_System is
        when WINDOWS => " > nul 2>&1",
        when LINUX => " > /dev/null 2>&1");

   Invocation_Exception : exception;

   function Execute_Command (Command : String) return Integer;

   procedure Execute_Command (Command : String);
   --  Throws invocation exception when return code is not equal to zero

end Commands;
