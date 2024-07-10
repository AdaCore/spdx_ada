with Ada.Containers.Vectors;

package SPDX is

   type Expression (<>) is private;

   function Parse (Str          : String;
                   Allow_Custom : Boolean := False)
                   return Expression;
   --  Parse an SPDX (v3.0) expression from string.
   --
   --  If Allow_Custom is True, the parser will accept custom license id with
   --  the format "custom-[0-9a-zA-Z.\-+]+", with the exception of the literal
   --  string "custom-+".

   function Valid (This : Expression) return Boolean;
   --  Return True if the SPDX expression is valid

   function Error (This : Expression) return String
     with Pre => not Valid (This);
   --  Return the error message for an invalid SPDX expression

   function Img (This : Expression) return String
     with Pre => Valid (This);
   --  Return the string representation of a valid SPDX expression

   function Has_Custom (This : Expression) return Boolean;
   --  Return True if the expression contains a custom license ID

private

   subtype Id_Characters is Character
     with Dynamic_Predicate => Id_Characters in
       'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.';

   subtype Id_String is String
     with Dynamic_Predicate => (for all C of Id_String => C in Id_Characters);

   subtype Whitespace_Characters is Character
     with Dynamic_Predicate => Whitespace_Characters in
       ' ' | ASCII.HT;

   type Token_Kind is (Op_Or_Later, Op_With, Op_Or, Op_And,
                       Id_Str, Paren_Open, Paren_Close, Colon,
                       DocumentRef, LicenseRef, AdditionRef);

   subtype Operator is Token_Kind range Op_Or_Later .. Op_And;

   type Location is record
      From, To : Natural;
   end record;

   type Token is record
      Kind : Token_Kind;
      Loc : Location; -- Position in the parsed string
   end record;

   package Token_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Token);

   type Error_Kind is (None,
                       Or_Later_Misplaced,
                       Colon_Misplaced,
                       Invalid_Char,
                       Operator_Mixed_Case,
                       Unexpected_Token,
                       Paren_Close_Expected,
                       License_Id_Expected,
                       Invalid_License_Id,
                       Addition_Expression_Expected,
                       Addition_Expression_Misplaced,
                       Invalid_Exception_Id,
                       DocumentRef_Missing_Colon,
                       DocumentRef_Missing_LicenseRef,
                       DocumentRef_Missing_AdditionRef,
                       Or_Later_In_User_Def_Ref,
                       Empty_Expression);

   type Expression (Str_Len : Natural) is record
      Str    : String (1 .. Str_Len);
      Tokens : Token_Vector.Vector;

      Error  : Error_Kind := None;
      Err_Loc : Location;

      Allow_Custom : Boolean := False;
      Has_Custom_Id : Boolean := False;
   end record;

   procedure Tokenize (This : in out Expression)
     with Pre => This.Tokens.Is_Empty;

end SPDX;
