with AUnit.Reporter.Text;
with AUnit.Run;
with Rejuvenation_Workshop_Suite; use Rejuvenation_Workshop_Suite;

procedure Test_Rejuvenation_Workshop is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Rejuvenation_Workshop;
