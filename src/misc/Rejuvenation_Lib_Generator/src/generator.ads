with Libadalang.Analysis;
with Libadalang.Common;
with Rejuvenation.Factory; use Rejuvenation.Factory;

package Generator is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Filename_LAL_Ads : constant String :=
     "C:\GNATPRO\22.1\include\libadalang\libadalang-analysis.ads";
   Filename_LALCO_Ads : constant String :=
     "C:\GNATPRO\22.1\include\libadalang\libadalang-common.ads";

   Unit_LAL_Ads : constant LAL.Analysis_Unit :=
     Open_File (Filename_LAL_Ads);
   Unit_LALCO_Ads : constant LAL.Analysis_Unit :=
     Open_File (Filename_LALCO_Ads);
end Generator;
