with Ada.Text_IO;                 use Ada.Text_IO;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Simple_Factory; use Rejuvenation.Simple_Factory;
with Rejuvenation.Utils;          use Rejuvenation.Utils;

package body Examples.Match_Patterns is

   -- Without placeholders --------

   procedure Demo_Without_Placeholders_Success;
   procedure Demo_Without_Placeholders_Failure;

   -- With single placeholders --------

   procedure Demo_With_Single_Placeholders_Success_Single_Occurrence;
   procedure Demo_With_Single_Placeholders_Success_Consistent_Occurrences;
   procedure Demo_With_Single_Placeholders_Failure_Inconsistent_Occurrences;
   procedure
   Demo_With_Single_Placeholders_Success_Consistent_Complex_Occurrences;

   -- With multiple placeholders --------

   procedure Demo_With_Multiple_Placeholders_Success_Single_Occurrence;
   procedure Demo_With_Multiple_Placeholders_Success_Consistent_Occurrences;
   procedure Demo_With_Multiple_Placeholders_Failure_Inconsistent_Occurrences;

   -- With single and multiple placeholders --------

   procedure Demo_With_Single_And_Multiple_Placeholders_Success;
   procedure Demo_With_Single_And_Multiple_Placeholders_Failure;

   procedure Demo is
   begin
      Put_Line ("=== Examples of Match_Pattern =======");
      New_Line;

      Put_Line ("--- Examples without placeholders (success) -------");
      New_Line;
      Demo_Without_Placeholders_Success;
      New_Line;

      Put_Line ("--- Examples without placeholders (failure) -------");
      New_Line;
      Demo_Without_Placeholders_Failure;
      New_Line;

      Put_Line
        ("--- Examples with single placeholders " &
         "(success: single occurence) -------");
      New_Line;
      Demo_With_Single_Placeholders_Success_Single_Occurrence;
      New_Line;

      Put_Line
        ("--- Examples with single placeholders " &
         "(success: consistent occurences) -------");
      New_Line;
      Demo_With_Single_Placeholders_Success_Consistent_Occurrences;
      New_Line;

      Put_Line
        ("--- Examples with single placeholders " &
         "(failure: inconsistent occurences) -------");
      New_Line;
      Demo_With_Single_Placeholders_Failure_Inconsistent_Occurrences;
      New_Line;

      Put_Line
        ("--- Examples with single placeholders " &
         "(success: consistent complex occurences) -------");
      New_Line;
      Demo_With_Single_Placeholders_Success_Consistent_Complex_Occurrences;
      New_Line;

      Put_Line
        ("--- Examples with multiple placeholders " &
         "(success: single occurence) -------");
      New_Line;
      Demo_With_Multiple_Placeholders_Success_Single_Occurrence;
      New_Line;

      Put_Line
        ("--- Examples with multiple placeholders " &
         "(success: consistent occurences) -------");
      New_Line;
      Demo_With_Multiple_Placeholders_Success_Consistent_Occurrences;
      New_Line;

      Put_Line
        ("--- Examples with multiple placeholders " &
         "(failure: inconsistent occurences) -------");
      New_Line;
      Demo_With_Multiple_Placeholders_Failure_Inconsistent_Occurrences;
      New_Line;

      Put_Line
        ("--- Examples with single and multiple placeholders " &
         "(success) -------");
      New_Line;
      Demo_With_Single_And_Multiple_Placeholders_Success;
      New_Line;

      Put_Line
        ("--- Examples with single and multiple placeholders " &
         "(failure) -------");
      New_Line;
      Demo_With_Single_And_Multiple_Placeholders_Failure;
      New_Line;

   end Demo;

   -- Without placeholders --------

   procedure Demo_Without_Placeholders_Success is
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern  => Analyze_Fragment ("x := 42;", Stmt_Rule).Root,
           Instance => Analyze_Fragment ("x := 42;", Stmt_Rule).Root);
      Put_Line (Result'Image);
   end Demo_Without_Placeholders_Success;

   procedure Demo_Without_Placeholders_Failure is
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Rejuvenation.Match_Patterns.DIAGNOSE :=
        True; -- To get diagnosis information
      Result :=
        MP.Match_Full
          (Pattern  => Analyze_Fragment ("x := 21;", Stmt_Rule).Root,
           Instance => Analyze_Fragment ("x := 42;", Stmt_Rule).Root);
      Rejuvenation.Match_Patterns.DIAGNOSE := False;
      Put_Line (Result'Image);
   end Demo_Without_Placeholders_Failure;

   -- With single placeholders --------

   procedure Demo_With_Single_Placeholders_Success_Single_Occurrence is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern  => Analyze_Fragment ("x := $S_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Single ("$S_value")'Image & " " &
         MP.Get_Single_As_Node ("$S_value").Image & " " &
         MP.Get_Single_As_Raw_Signature ("$S_value"));
   end Demo_With_Single_Placeholders_Success_Single_Occurrence;

   procedure Demo_With_Single_Placeholders_Success_Consistent_Occurrences is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42 + 42;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment ("x := $S_value + $S_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Single ("$S_value")'Image & " " &
         MP.Get_Single_As_Node ("$S_value").Image & " " &
         MP.Get_Single_As_Raw_Signature ("$S_value"));
   end Demo_With_Single_Placeholders_Success_Consistent_Occurrences;

   procedure Demo_With_Single_Placeholders_Failure_Inconsistent_Occurrences is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42 + 21;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Rejuvenation.Match_Patterns.DIAGNOSE :=
        True; -- To get diagnosis information
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment ("x := $S_value + $S_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Rejuvenation.Match_Patterns.DIAGNOSE := False;
      Put_Line (Result'Image & " " & MP.Has_Single ("$S_value")'Image);
   end Demo_With_Single_Placeholders_Failure_Inconsistent_Occurrences;

   procedure
   Demo_With_Single_Placeholders_Success_Consistent_Complex_Occurrences
   is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := (f(a)+12) + (f(a)+12);", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment ("x := $S_value + $S_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Single ("$S_value")'Image & " " &
         MP.Get_Single_As_Node ("$S_value").Image & " " &
         MP.Get_Single_As_Raw_Signature ("$S_value"));
   end Demo_With_Single_Placeholders_Success_Consistent_Complex_Occurrences;

   -- With multiple placeholders --------

   procedure Demo_With_Multiple_Placeholders_Success_Single_Occurrence is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern  => Analyze_Fragment ("x := $M_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Multiple ("$M_value")'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_value").Length'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_value") (1).Image);
   end Demo_With_Multiple_Placeholders_Success_Single_Occurrence;

   procedure Demo_With_Multiple_Placeholders_Success_Consistent_Occurrences is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42 + 42;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment ("x := $M_value + $M_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Multiple ("$M_value")'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_value").Length'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_value") (1).Image);
   end Demo_With_Multiple_Placeholders_Success_Consistent_Occurrences;

   procedure Demo_With_Multiple_Placeholders_Failure_Inconsistent_Occurrences
   is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42 + 21;", Stmt_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Rejuvenation.Match_Patterns.DIAGNOSE :=
        True; -- To get diagnosis information
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment ("x := $M_value + $M_value;", Stmt_Rule).Root,
           Instance => Unit.Root);
      Rejuvenation.Match_Patterns.DIAGNOSE := False;
      Put_Line (Result'Image & " " & MP.Has_Multiple ("$M_value")'Image);
   end Demo_With_Multiple_Placeholders_Failure_Inconsistent_Occurrences;

   -- With single and multiple placeholders --------

   procedure Demo_With_Single_And_Multiple_Placeholders_Success is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42; x := 21; y := 22; x := 42;", Stmts_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment
               ("x := $S_value; $M_stats; x := $S_value;", Stmts_Rule)
               .Root,
           Instance => Unit.Root);
      Put_Line
        (Result'Image & " " & MP.Has_Single ("$S_value")'Image & " " &
         MP.Get_Single_As_Node ("$S_value").Image & " " &
         MP.Get_Single_As_Raw_Signature ("$S_value") & " " &
         MP.Has_Multiple ("$M_stats")'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_stats").Length'Image & " " &
         MP.Get_Multiple_As_Nodes ("$M_stats") (1).Image & " " &
         Rejuvenation.Utils.Raw_Signature
           (MP.Get_Multiple_As_Nodes ("$M_stats") (1)) &
         " " & MP.Get_Multiple_As_Nodes ("$M_stats") (2).Image & " " &
         Rejuvenation.Utils.Raw_Signature
           (MP.Get_Multiple_As_Nodes ("$M_stats") (2)));
   end Demo_With_Single_And_Multiple_Placeholders_Success;

   procedure Demo_With_Single_And_Multiple_Placeholders_Failure is
      --  To be able to access placeholder nodes, do not inline Unit.
      Unit : constant Analysis_Unit :=
        Analyze_Fragment ("x := 42; y := 21; y := 21; x := 43;", Stmts_Rule);
      MP     : Match_Pattern;
      Result : Boolean;
   begin
      Rejuvenation.Match_Patterns.DIAGNOSE :=
        True; -- To get diagnosis information
      Result :=
        MP.Match_Full
          (Pattern =>
             Analyze_Fragment
               ("x := $S_value; $M_stats; x := $S_value;", Stmts_Rule)
               .Root,
           Instance => Unit.Root);
      Rejuvenation.Match_Patterns.DIAGNOSE := False;
      Put_Line
        (Result'Image & " " & MP.Has_Single ("$S_value")'Image & " " &
         MP.Has_Multiple ("$M_stats")'Image);
   end Demo_With_Single_And_Multiple_Placeholders_Failure;

end Examples.Match_Patterns;
