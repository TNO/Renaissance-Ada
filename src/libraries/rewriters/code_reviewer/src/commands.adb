with Interfaces.C; use Interfaces.C;

package body Commands is

   function Sys (Arg : char_array) return Integer;
   pragma Import (C, Sys, "system");

   function Execute_Command (Command : String) return Integer is
   begin
      return Sys (To_C (Command));
   end Execute_Command;

   procedure Execute_Command (Command : String) is
      Ret_Val : constant Integer := Execute_Command (Command);
   begin
      if Ret_Val /= 0 then
         raise Invocation_Exception
           with Ret_Val'Image & " for '" & Command & "'";
      end if;
   end Execute_Command;

end Commands;
