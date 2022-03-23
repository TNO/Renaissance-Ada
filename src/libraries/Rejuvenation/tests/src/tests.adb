with AUnit.Reporter.Text;
with AUnit.Run;
with Rejuvenation_Suite; use Rejuvenation_Suite;

procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Tests;
