with Ada.Assertions;        use Ada.Assertions;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Rejuvenation.Nested is

   function Remove_Nested_Flags
     (Source : String; On_Flag : String; Off_Flag : String;
      Depth  : Natural := 0) return String
   is

      function Next_Flag_Location
        (From : Positive; Flag : String) return Positive;
      function Next_Flag_Location
        (From : Positive; Flag : String) return Positive
      is
         I : constant Natural := Index (Source, Flag, From);
      begin
         return (if I = 0 then Source'Last + 1 else I);
      end Next_Flag_Location;

      function Next_On_Flag_Location (From : Positive) return Positive is
        (Next_Flag_Location (From, On_Flag));

      function Next_Off_Flag_Location (From : Positive) return Positive is
        (Next_Flag_Location (From, Off_Flag));

      Current_Location : Natural          := Source'First;
      Current_Depth    : Natural          := Depth;
      On_Location      : Positive := Next_On_Flag_Location (Current_Location);
      Off_Location     : Positive := Next_Off_Flag_Location (Current_Location);
      Final            : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         if On_Location > Source'Last and then Off_Location > Source'Last then
            Assert
              (Check   => Current_Depth = 0,
               Message =>
                 "Unexpectedly at Current_Depth " & Current_Depth'Image);

            Append (Final, Source (Current_Location .. Source'Last));
            return To_String (Final);
         elsif On_Location < Off_Location then
            declare
               Next_Location : constant Positive :=
                 On_Location + On_Flag'Length;
               Last : constant Positive :=
                 (if Current_Depth = 0 then Next_Location - 1
                  else On_Location - 1);
            begin
               Current_Depth := Current_Depth + 1;
               Append (Final, Source (Current_Location .. Last));
               Current_Location := Next_Location;
               Assert
                 (Check   => Current_Location <= Off_Location,
                  Message => "Invariant violated");
               On_Location := Next_On_Flag_Location (Current_Location);
            end;
         else
            Assert
              (Check   => Off_Location < On_Location,
               Message => "On and Off token can't occur at same location");
            declare
               Next_Location : constant Positive :=
                 Off_Location + Off_Flag'Length;
               Last : constant Positive :=
                 (if Current_Depth = 1 then Next_Location - 1
                  else Off_Location - 1);
            begin
               Assert
                 (Check   => Current_Depth > 0,
                  Message =>
                    "Current_Depth is zero at offset " & Off_Location'Image);
               Current_Depth := Current_Depth - 1;
               Append (Final, Source (Current_Location .. Last));
               Current_Location := Next_Location;
               Assert
                 (Check   => Current_Location <= On_Location,
                  Message => "Invariant violated");
               Off_Location := Next_Off_Flag_Location (Current_Location);
            end;
         end if;
      end loop;
   end Remove_Nested_Flags;
end Rejuvenation.Nested;
