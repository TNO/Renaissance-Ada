package body Rewriters_Find_And_Replace_Combined is

      overriding function Get_Match_Accepter
     (RFR : Rewriters_Find_And_Replace_Combined)
      return Match_Accepter'Class is
     (RFR.A_Match_Accepter.all);


end Rewriters_Find_And_Replace_Combined;
