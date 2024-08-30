--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with SPDX;

procedure Main is

   Fail_Cnt : Natural := 0;
   Pass_Cnt : Natural := 0;

   procedure Test (Str            : String;
                   Expected_Error : String := "";
                   Allow_Custom   : Boolean := False);

   procedure Test (Str            : String;
                   Expected_Error : String := "";
                   Allow_Custom   : Boolean := False)
   is
   begin
      declare
         Exp : constant SPDX.Expression := SPDX.Parse (Str, Allow_Custom);
         Error : constant String :=
           (if SPDX.Valid (Exp) then "" else SPDX.Error (Exp));
      begin
         if Error /= Expected_Error then
            Put_Line ("FAIL: '" & Str & "'");
            if Expected_Error /= "" then
               Put_Line ("   Expected error: '" & Expected_Error & "'");
               Put_Line ("         but got : '" & Error & "'");
            else
               Put_Line ("   Unexpected error: '" & Error & "'");
            end if;

            Fail_Cnt := Fail_Cnt + 1;

         elsif Expected_Error = ""
             and then
               Allow_Custom
             and then
               not SPDX.Has_Custom (Exp)
         then
            Put_Line ("FAIL: '" & Str & "'");
            Put_Line ("   Has_Custom returned False");
            Fail_Cnt := Fail_Cnt + 1;
         else
            Put_Line ("PASS: '" & Str & "'");
            Pass_Cnt := Pass_Cnt + 1;
         end if;
      end;
   exception
      when E : others =>
         Put_Line ("FAIL: '" & Str & "'");
         Put_Line ("    With exception: '" &
                     Ada.Exceptions.Exception_Information (E) & "'");
         Fail_Cnt := Fail_Cnt + 1;
   end Test;

begin

   --  Test all invalid chars
   for C in Character loop
      if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
                  '-' | '.' | '(' | ')' | '+' | ':' | ' ' | ASCII.HT
      then
         Test ("test" & C, "Invalid character at 5");
      end if;
   end loop;

   Test ("test:", ": operator must follow a valid DocumentRef (5:5)");

   Test ("", "Empty license expression at (0:0)");
   Test ("test-3", "Invalid license ID: 'test-3' (1:6)");
   Test ("test-3.0", "Invalid license ID: 'test-3.0' (1:8)");
   Test ("MIT");
   Test ("MIT+");
   Test ("MIT OR MIT");
   Test ("MIT or MIT");
   Test ("MIT AND MIT");
   Test ("MIT Or MIT", "Mixed case operator at (5:6)");
   Test ("MIT anD MIT", "Mixed case operator at (5:7)");
   Test ("MIT and MIT");
   Test ("MIT WITH AND", "Addition expression expected at (10:12)");
   Test ("MIT WITH", "Addition expression expected at (8:8)");
   Test ("MIT WITH plop", "Invalid license exception ID: 'plop' (10:13)");
   Test ("MIT WITH GPL-3.0-linking-exception");
   Test ("MIT with GPL-3.0-linking-exception");
   Test ("MIT WiTh GPL-3.0-linking-exception", "Mixed case operator at (5:8)");
   Test ("(MIT)");
   Test ("(MIT) AND MIT");
   Test ("(MIT+) AND (MIT)");
   Test ("((MIT) AND (MIT+))");
   Test ("((MIT) AND (MIT+ OR MIT AND MIT AND (MIT WITH GPL-3.0-linking-exception AND MIT)))");

   Test ("MIT +", "+ operator must follow and indentifier without whitespace (5:5)");
   Test ("MIT AND +", "+ operator must follow and indentifier without whitespace (9:9)");
   Test ("MIT+AND", "Invalid license ID: 'MIT+AND' (1:7)");

   Test ("MIT AND", "Empty license expression at (7:7)");
   Test ("MIT OR", "Empty license expression at (6:6)");
   Test ("MIT MIT", "Unexpected token at (5:7)");

   Test ("(MIT", "Missing closing parentheses ')' at (4:4)");
   Test ("MIT)", "Unexpected token at (4:4)");
   Test ("(MIT AND (MIT OR MIT)", "Missing closing parentheses ')' at (21:21)");
   Test ("MIT AND (MIT OR MIT))", "Unexpected token at (21:21)");

   Test ("custom-plop", "Invalid license ID: 'custom-plop' (1:11)", Allow_Custom => False);
   Test ("custom", "Invalid license ID: 'custom' (1:6)", Allow_Custom => True);
   Test ("custom-", "Invalid license ID: 'custom-' (1:7)", Allow_Custom => True);
   Test ("custom-plop", Allow_Custom => True);
   Test ("custom-plop+", Allow_Custom => True);
   Test ("custom-plo+p", Allow_Custom => True);
   Test ("custom-+", "Invalid license ID: 'custom-' (1:7)", Allow_Custom => True);
   Test ("custom-++", Allow_Custom => True);
   Test ("custom-+a+", Allow_Custom => True);
   Test ("custom-+a", Allow_Custom => True);
   Test ("custom-test:test", ": operator must follow a valid DocumentRef (12:12)", Allow_Custom => True);
   Test ("CuStoM-test-1.0.3", Allow_Custom => True);
   Test ("custom-test AND custom-plop", Allow_Custom => True);

   Test ("custom-test WITH GPL-3.0-linking-exception", Allow_Custom => True);
   Test ("custom-test WITH custom-plop", "Invalid license exception ID: 'custom-plop' (18:28)", Allow_Custom => True);
   Test ("MIT WITH custom-plop", "Invalid license exception ID: 'custom-plop' (10:20)", Allow_Custom => True);

   Test ("LicenseRef-plop");
   Test ("LicenseRef", "Invalid license ID: 'LicenseRef' (1:10)");
   Test ("LicenseRef-", "Invalid license ID: 'LicenseRef-' (1:11)");
   Test ("LicenseRef-plop+", "+ operator in user defined reference: 'LicenseRef-plop+' (1:16)"); -- Note that SPDX specification differs from the "custom-*" implementation in this respect
   Test ("LicenseRef-plo+p", "+ operator in user defined reference: 'LicenseRef-plo+p' (1:16)");
   Test ("LicenseRef-test:test", ": operator must follow a valid DocumentRef (16:16)");
   Test ("LiCenSereF-test-1.0.3");
   Test ("LicenseRef-test AND LicenseRef-plop");

   Test ("DocumentRef-foo:LicenseRef-bar");
   Test ("DocumentRef", "Invalid license ID: 'DocumentRef' (1:11)");
   Test ("DocumentRef-", "Invalid license ID: 'DocumentRef-' (1:12)");
   Test ("DocumentRef-foo", "No ':' following DocumentRef: 'DocumentRef-foo' (1:15)");
   Test ("DocumentRef-foo:", "No LicenseRef identifier following DocumentRef: 'DocumentRef-foo:' (1:16)");
   Test ("DocumentRef-foo:bar", "No LicenseRef identifier following DocumentRef: 'DocumentRef-foo:bar' (1:19)");

   Test ("AdditionRef-plop", "Addition expression must follow a 'WITH' operator (1:16)");
   Test ("MIT WITH AdditionRef-plop");
   Test ("MIT WITH plop", "Invalid license exception ID: 'plop' (10:13)");
   Test ("MIT WITH AdditionRef-test:test", ": operator must follow a valid DocumentRef (26:26)");

   Test ("MIT WITH DocumentRef-foo:AdditionRef-bar");
   Test ("MIT WITH DocumentRef-", "Invalid license exception ID: 'DocumentRef-' (10:21)");
   Test ("MIT WITH DocumentRef-foo", "No ':' following DocumentRef: 'DocumentRef-foo' (10:24)");
   Test ("MIT WITH DocumentRef-foo:", "No AdditionRef identifier following DocumentRef: 'DocumentRef-foo:' (10:25)");
   Test ("MIT WITH DocumentRef-foo:LicenseRef-bar", "No AdditionRef identifier following DocumentRef: 'DocumentRef-foo:LicenseRef-bar' (10:39)");
   Test ("MIT WITH DocumentRef:AdditionRef-bar", ": operator must follow a valid DocumentRef (21:21)");
   Test ("MIT WITH DocumentRef-:AdditionRef-bar", ": operator must follow a valid DocumentRef (22:22)");

   Test ("LicenseRef-plop WITH GPL-3.0-linking-exception");
   Test ("LicenseRef-foo WITH AdditionRef-bar");
   Test ("DocumentRef-foo:LicenseRef-bar WITH DocumentRef-abc:AdditionRef-xyz");

   Put_Line ("PASS:" & Pass_Cnt'Img);
   Put_Line ("FAIL:" & Fail_Cnt'Img);

   if Fail_Cnt /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
