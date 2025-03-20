--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with SPDX.Licenses;
with SPDX.Exceptions;

package body SPDX is

   function Token_Str (This : Expression; Loc : Location) return String;

   function Contains_Plus (Str : String) return Boolean;
   function Has_Prefix (Str : String; Prefix : String) return Boolean;
   function Is_Custom_Id (Str : String) return Boolean;
   procedure Parse_License (This : in out Expression);
   procedure Parse_Compound_Expression (This : in out Expression);
   procedure Parse_Simple_Expression (This : in out Expression);
   procedure Parse_Addition_Expression (This : in out Expression);

   ---------------
   -- Token_Str --
   ---------------

   function Token_Str (This : Expression; Loc : Location) return String is
   begin
      if Loc.From not in This.Str'Range
        or else
          Loc.To not in This.Str'Range
      then
         return "";
      else
         return This.Str (Loc.From .. Loc.To);
      end if;
   end Token_Str;

   -------------------
   -- Contains_Plus --
   -------------------

   function Contains_Plus (Str : String) return Boolean is
   begin
      for I in Str'Range loop
         if Str (I) = '+' then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Plus;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (Str : String; Prefix : String) return Boolean is
      Str_Lower : constant String := To_Lower (Str);
      Prfx_Lower : constant String := To_Lower (Prefix);
   begin
      return Str_Lower'Length > Prfx_Lower'Length
        and then
          Str_Lower (Str_Lower'First .. Str_Lower'First + Prfx_Lower'Length - 1)
            = Prfx_Lower;
   end Has_Prefix;

   ------------------
   -- Is_Custom_Id --
   ------------------

   function Is_Custom_Id (Str : String) return Boolean is
      Prefix : constant String := "custom-";
   begin
      return Has_Prefix (Str, Prefix);
   end Is_Custom_Id;

   -------------------
   -- Parse_License --
   -------------------

   procedure Parse_License (This : in out Expression) is
   begin
      Parse_Compound_Expression (This);

      if This.Error /= None then
         return;
      end if;

      if not This.Tokens.Is_Empty then
         This.Error := Unexpected_Token;
         This.Err_Loc := This.Tokens.First_Element.Loc;
      end if;
   end Parse_License;

   -------------------------------
   -- Parse_Compound_Expression --
   -------------------------------

   procedure Parse_Compound_Expression (This : in out Expression) is
   begin
      --  compound =   ( compound )
      --             | simple
      --             | simple WITH addition
      --             | compound AND compound
      --             | compound OR compound
      --
      --  i.e.
      --    compound =   x
      --               | x AND compound
      --               | x OR compound
      --  where
      --    x =   ( compound )
      --        | simple
      --        | simple WITH addition

      if This.Tokens.Is_Empty then
         This.Error := Empty_Expression;
         This.Err_Loc := (This.Str'Last, This.Str'Last);
         return;
      end if;

      case This.Tokens.First_Element.Kind is

      when Paren_Open =>

         This.Tokens.Delete_First;

         Parse_Compound_Expression (This);

         if This.Error /= None then
            return;
         end if;

         if This.Tokens.Is_Empty then
            This.Error := Paren_Close_Expected;
            This.Err_Loc := (This.Str'Last, This.Str'Last);
            return;
         end if;

         if This.Tokens.First_Element.Kind /= Paren_Close then
            This.Error := Paren_Close_Expected;
            This.Err_Loc := This.Tokens.First_Element.Loc;
            return;
         end if;

         --  Delete the Paren_Close
         This.Tokens.Delete_First;

      when Id_Str | DocumentRef | LicenseRef | AdditionRef =>
         Parse_Simple_Expression (This);

         if This.Error /= None then
            return;
         end if;

         if not This.Tokens.Is_Empty
           and then This.Tokens.First_Element.Kind = Op_With
         then
            This.Tokens.Delete_First;

            Parse_Addition_Expression (This);

            if This.Error /= None then
               return;
            end if;

         end if;

      when others =>
         This.Error := Unexpected_Token;
         This.Err_Loc := This.Tokens.First_Element.Loc;
         return;
      end case;

      if This.Tokens.Is_Empty then
         --  End of expression
         return;
      end if;

      if This.Tokens.First_Element.Kind in Op_And | Op_Or then
         --  Just skip operator as we do not build the AST
         This.Tokens.Delete_First;

         Parse_Compound_Expression (This);
      end if;
   end Parse_Compound_Expression;

   -----------------------------
   -- Parse_Simple_Expression --
   -----------------------------

   procedure Parse_Simple_Expression (This : in out Expression) is
      From : constant Natural := This.Tokens.First_Element.Loc.From;
      First_Token_As_Str : constant String :=
         Token_Str (This, This.Tokens.First_Element.Loc);
   begin
      --  simple =   id
      --           | id+
      --           | license-ref

      if This.Tokens.Is_Empty then
         This.Error := License_Id_Expected;
         This.Err_Loc := (This.Str'Last, This.Str'Last);
         return;
      end if;

      if This.Tokens.First_Element.Kind = Id_Str then
         if This.Allow_Custom and then Is_Custom_Id (First_Token_As_Str) then
            This.Has_Custom_Id := True;
         elsif not SPDX.Licenses.Valid_Id (First_Token_As_Str) then
            This.Error := Invalid_License_Id;
            This.Err_Loc := This.Tokens.First_Element.Loc;
         end if;

         This.Tokens.Delete_First;

         --  + operator
         if not This.Tokens.Is_Empty
            and then This.Tokens.First_Element.Kind = Op_Or_Later
         then
            This.Tokens.Delete_First;
         end if;

      elsif This.Tokens.First_Element.Kind = DocumentRef then
         --  Must have the form "DocumentRef-*:LicenseRef-*"
         This.Tokens.Delete_First;
         if This.Tokens.Is_Empty then
            This.Error := DocumentRef_Missing_LicenseRef;
            This.Err_Loc := (From, This.Str'Last);
         elsif This.Tokens.First_Element.Kind /= Colon then
            This.Error := DocumentRef_Missing_LicenseRef;
            This.Err_Loc := (From, This.Tokens.First_Element.Loc.From);
         else
            This.Tokens.Delete_First;
            if This.Tokens.Is_Empty then
               This.Error := DocumentRef_Missing_LicenseRef;
               This.Err_Loc := (From, This.Str'Last);
            elsif This.Tokens.First_Element.Kind /= LicenseRef then
               This.Error := DocumentRef_Missing_LicenseRef;
               This.Err_Loc := (From, This.Tokens.First_Element.Loc.To);
            end if;
         end if;

         This.Tokens.Delete_First;

      elsif This.Tokens.First_Element.Kind = LicenseRef then
         This.Tokens.Delete_First;

      elsif This.Tokens.First_Element.Kind = AdditionRef then
         This.Error := Addition_Expression_Misplaced;
         This.Err_Loc := This.Tokens.First_Element.Loc;

      else
         This.Error := License_Id_Expected;
         This.Err_Loc := This.Tokens.First_Element.Loc;
      end if;

   end Parse_Simple_Expression;

   -------------------------------
   -- Parse_Addition_Expression --
   -------------------------------

   procedure Parse_Addition_Expression (This : in out Expression) is
      From : constant Natural := This.Tokens.First_Element.Loc.From;
      First_Token_As_Str : constant String :=
         Token_Str (This, This.Tokens.First_Element.Loc);
   begin
      --  addition =    license-exception-id
      --              | addition-ref

      if This.Tokens.Is_Empty then
         This.Error := Addition_Expression_Expected;
         This.Err_Loc := (This.Str'Last, This.Str'Last);
         return;
      end if;

      if This.Tokens.First_Element.Kind = Id_Str then
         if not SPDX.Exceptions.Valid_Id (First_Token_As_Str) then
            This.Error := Invalid_Exception_Id;
            This.Err_Loc := This.Tokens.First_Element.Loc;
         end if;

         This.Tokens.Delete_First;

      elsif This.Tokens.First_Element.Kind = DocumentRef then
         --  Must have the form "DocumentRef-*:AdditionRef-*"
         This.Tokens.Delete_First;
         if This.Tokens.Is_Empty then
            This.Error := DocumentRef_Missing_AdditionRef;
            This.Err_Loc := (From, This.Str'Last);
         elsif This.Tokens.First_Element.Kind /= Colon then
            This.Error := DocumentRef_Missing_AdditionRef;
            This.Err_Loc := (From, This.Tokens.First_Element.Loc.From);
         else
            This.Tokens.Delete_First;
            if This.Tokens.Is_Empty then
               This.Error := DocumentRef_Missing_AdditionRef;
               This.Err_Loc := (From, This.Str'Last);
            elsif This.Tokens.First_Element.Kind /= AdditionRef then
               This.Error := DocumentRef_Missing_AdditionRef;
               This.Err_Loc := (From, This.Tokens.First_Element.Loc.To);
            end if;
         end if;

         This.Tokens.Delete_First;

      elsif This.Tokens.First_Element.Kind = AdditionRef then
         This.Tokens.Delete_First;

      else
         This.Error := Addition_Expression_Expected;
         This.Err_Loc := This.Tokens.First_Element.Loc;
         return;
      end if;
   end Parse_Addition_Expression;

   -----------
   -- Parse --
   -----------

   function Parse (Str          : String;
                   Allow_Custom : Boolean := False)
                   return Expression
   is
      Exp : Expression (Str'Length);
   begin

      Exp.Str := Str;
      Exp.Allow_Custom := Allow_Custom;

      Tokenize (Exp);

      if Exp.Error /= None then
         return Exp;
      end if;

      Parse_License (Exp);

      return Exp;
   end Parse;

   -----------
   -- Error --
   -----------

   function Error (This : Expression) return String is

      function Img (N : Natural) return String;
      function Img (Loc : Location) return String;

      ---------
      -- Img --
      ---------

      function Img (N : Natural) return String
      is (Ada.Strings.Fixed.Trim (N'Img, Ada.Strings.Left));

      ---------
      -- Img --
      ---------

      function Img (Loc : Location) return String
      is ((Img (Loc.From) & ":" & Img (Loc.To)));

   begin
      case This.Error is
         when None =>
            return "";

         when Or_Later_Misplaced =>
            return "+ operator must follow and indentifier without " &
              "whitespace (" & Img (This.Err_Loc) & ")";

         when Colon_Misplaced =>
            return ": operator must follow a valid DocumentRef (" &
              Img (This.Err_Loc) & ")";

         when Invalid_Char =>
            return "Invalid character at " & Img (This.Err_Loc.From);

         when Operator_Mixed_Case =>
            return "Mixed case operator at (" & Img (This.Err_Loc) & ")";

         when Unexpected_Token =>
            return "Unexpected token at (" & Img (This.Err_Loc) & ")";

         when Paren_Close_Expected =>
            return "Missing closing parentheses ')' at (" &
              Img (This.Err_Loc) & ")";

         when License_Id_Expected =>
            return "License id expected at (" & Img (This.Err_Loc) & ")";

         when Invalid_License_Id =>
            return "Invalid license ID: '" &
              Token_Str (This, This.Err_Loc) & "' (" & Img (This.Err_Loc) & ")";

         when Addition_Expression_Expected =>
            return "Addition expression expected at (" &
              Img (This.Err_Loc) & ")";

         when Addition_Expression_Misplaced =>
            return "Addition expression must follow a 'WITH' operator (" &
              Img (This.Err_Loc) & ")";

         when Invalid_Exception_Id =>
            return "Invalid license exception ID: '" &
              Token_Str (This, This.Err_Loc) &
              "' (" & Img (This.Err_Loc) & ")";

         when DocumentRef_Missing_LicenseRef =>
            return "No LicenseRef identifier following DocumentRef: '" &
              Token_Str (This, This.Err_Loc) &
              "' (" & Img (This.Err_Loc) & ")";

         when DocumentRef_Missing_AdditionRef =>
            return "No AdditionRef identifier following DocumentRef: '" &
              Token_Str (This, This.Err_Loc) &
              "' (" & Img (This.Err_Loc) & ")";

         when DocumentRef_Missing_Colon =>
            return "No ':' following DocumentRef: '" &
              Token_Str (This, This.Err_Loc) &
              "' (" & Img (This.Err_Loc) & ")";

         when Or_Later_In_User_Def_Ref =>
            return "+ operator in user defined reference: '" &
              Token_Str (This, This.Err_Loc) &
              "' (" & Img (This.Err_Loc) & ")";

         when Empty_Expression =>
            return "Empty license expression at (" & Img (This.Err_Loc) & ")";

      end case;
   end Error;

   -----------
   -- Valid --
   -----------

   function Valid (This : Expression) return Boolean is
   begin
      return This.Error = None;
   end Valid;

   ---------
   -- Img --
   ---------

   function Img (This : Expression) return String is
   begin
      return This.Str;
   end Img;

   ----------------
   -- Has_Custom --
   ----------------

   function Has_Custom (This : Expression) return Boolean
   is (This.Has_Custom_Id);

   --------------
   -- Tokenize --
   --------------

   procedure Tokenize (This : in out Expression) is

      Tokens : Token_Vector.Vector renames This.Tokens;
      Str : String renames This.Str;

      Index : Natural := Str'First;

   begin
      while Index in Str'Range loop

         if Str (Index) in Whitespace_Characters then
            Index := Index + 1; -- Skip whitespace

         elsif Str (Index) = '(' then
            Tokens.Append ((Paren_Open, (Index, Index)));
            Index := Index + 1;

         elsif Str (Index) = ')' then
            Tokens.Append ((Paren_Close, (Index, Index)));
            Index := Index + 1;

         elsif Str (Index) = ':' then
            This.Error := Colon_Misplaced;
            This.Err_Loc := (Index, Index);
            return;

         elsif Str (Index) = '+' then
            This.Error := Or_Later_Misplaced;
            This.Err_Loc := (Index, Index);
            return;

         elsif Str (Index) in Id_Characters then

            --  Operator or identifier

            declare
               From : constant Natural := Index;
            begin
               while Index in Str'Range
                 and then Str (Index) in Id_Characters | '+'
               loop
                  Index := Index + 1;
               end loop;

               declare
                  To            : constant Natural := Index - 1;
                  Substr        : constant String := Str (From .. To);
                  DocRef_Prefix : constant String := "DocumentRef-";
                  LicRef_Prefix : constant String := "LicenseRef-";
                  AddRef_Prefix : constant String := "AdditionRef-";
               begin
                  if Substr = "WITH" or else Substr = "with" then
                     Tokens.Append ((Op_With, (From, To)));

                  elsif Substr = "OR" or else Substr = "or" then
                     Tokens.Append ((Op_Or, (From, To)));

                  elsif Substr = "AND" or else Substr = "and" then
                     Tokens.Append ((Op_And, (From, To)));

                  elsif To_Lower (Substr) = "with"
                    or else To_Lower (Substr) = "or"
                    or else To_Lower (Substr) = "and"
                  then
                     This.Error := Operator_Mixed_Case;
                     This.Err_Loc := (From, To);
                     return;

                  elsif Has_Prefix (Substr, DocRef_Prefix)
                    and then Substr'Length > DocRef_Prefix'Length -- needs an id
                  then
                     if Contains_Plus (Substr) then
                        This.Error := Or_Later_In_User_Def_Ref;
                        This.Err_Loc := (From, To);
                        return;
                     end if;

                     --  : must follow a document reference (without whitespace)
                     if To = Str'Last or else Str (To + 1) /= ':' then
                        This.Error := DocumentRef_Missing_Colon;
                        This.Err_Loc := (From, To);
                        return;
                     end if;

                     Tokens.Append ((DocumentRef, (From, To)));
                     Tokens.Append ((Colon, (To + 1, To + 1)));
                     Index := Index + 1;

                  elsif Has_Prefix (Substr, LicRef_Prefix)
                    and then Substr'Length > LicRef_Prefix'Length
                  then
                     if Contains_Plus (Substr) then
                        This.Error := Or_Later_In_User_Def_Ref;
                        This.Err_Loc := (From, To);
                        return;
                     end if;
                     Tokens.Append ((LicenseRef, (From, To)));

                  elsif Has_Prefix (Substr, AddRef_Prefix)
                    and then Substr'Length > AddRef_Prefix'Length
                  then
                     if Contains_Plus (Substr) then
                        This.Error := Or_Later_In_User_Def_Ref;
                        This.Err_Loc := (From, To);
                        return;
                     end if;
                     Tokens.Append ((AdditionRef, (From, To)));

                  else
                     if Str (To) = '+' then
                        --  + operator can be found after and id (without
                        --  whitespace).
                        Tokens.Append ((Id_Str, (From, To - 1)));
                        Tokens.Append ((Op_Or_Later, (To, To)));
                     else
                        Tokens.Append ((Id_Str, (From, To)));
                     end if;
                  end if;
               end;
            end;

         else
            This.Error := Invalid_Char;
            This.Err_Loc := (Index, Index);
            return;

         end if;
      end loop;
   end Tokenize;
end SPDX;
