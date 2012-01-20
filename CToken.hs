module CToken where

import Data.Int
import Data.Word

type SourceCode = String

type Type_Char  = Int8
type Type_UChar = Word8
type Type_Short  = Int16
type Type_UShort = Word16
type Type_Int  = Int32
type Type_UInt = Word32
type Type_Long  = Type_Int
type Type_ULong = Type_UInt


data CNumber = Num_Char Type_Char
             | Num_UChar Type_UChar
             | Num_Short Type_Short
             | Num_UShort Type_UShort
             | Num_Int Type_Int
             | Num_UInt Type_UInt
             | Num_Long Type_Long
             | Num_ULong Type_ULong
             | Num_Float Double
             | Num_Double Double


data CSymbol = Sym_LP -- ^ (
             | Sym_RP -- ^ )
             | Sym_COMMA -- ^ ,
             | Sym_LC -- ^ {
             | Sym_RC -- ^ }
             | Sym_LB -- ^ \[
             | Sym_RB -- ^ ]
             | Sym_DOT -- ^ .
             | Sym_AND -- ^ &
             | Sym_STAR -- ^ \*
             | Sym_PLUS -- ^ +
             | Sym_MINUS -- ^ \-
             | Sym_NEGATE -- ^ ~
             | Sym_NOT -- ^ !
             | Sym_DIV -- ^ \/
             | Sym_MOD -- ^ %
             | Sym_LT -- ^ <
             | Sym_GT -- ^ \>
             | Sym_XOR -- ^ ^
             | Sym_OR -- ^ |
             | Sym_QUESTION -- ^ ?
             | Sym_COLON -- ^ :
             | Sym_SEMICOLON -- ^ ;
             | Sym_ASSIGN -- ^ =
             | Sym_ARROW -- ^ \->
             | Sym_ICR -- ^ ++
             | Sym_DECR -- ^ \--
             | Sym_LS -- ^ <<
             | Sym_RS -- ^ \>>
             | Sym_LE -- ^ <=
             | Sym_GE -- ^ \>=
             | Sym_EQ -- ^ ==
             | Sym_NE -- ^ !=
             | Sym_ANDAND -- ^ &&
             | Sym_OROR -- ^ ||
             | Sym_MULT_ASSIGN -- ^ \*=
             | Sym_DIV_ASSIGN -- ^ \/=
             | Sym_MOD_ASSIGN -- ^ %=
             | Sym_PLUS_ASSIGN -- ^ +=
             | Sym_MINUS_ASSIGN -- ^ \-=
             | Sym_LS_ASSIGN -- ^ <<=
             | Sym_RS_ASSIGN -- ^ \>>=
             | Sym_AND_ASSIGN -- ^ &=
             | Sym_OR_ASSIGN -- ^ |=
             | Sym_XOR_ASSIGN -- ^ |=
             | Sym_ELLIPSIS -- ^ ...


data CKeyword = Key_EXTERN
              | Key_STATIC

              | Key_SIZEOF
              | Key_TYPEDEF
              | Key_ENUM
              | Key_STRUCT
              | Key_UNION

              | Key_VOID
              | Key_SIGNED
              | Key_UNSIGNED
              | Key_CHAR
              | Key_SHORT
              | Key_INT
              | Key_LONG
              | Key_FLOAT
              | Key_DOUBLE

              | Key_FOR
              | Key_DO
              | Key_WHILE
              | Key_CONTINUE
              | Key_BREAK
              | Key_SWITCH
              | Key_CASE
              | Key_DEFAULT
              | Key_IF
              | Key_ELSE

              | Key_GOTO
              | Key_RETURN

              | Key_AUTO
              | Key_REGISTER


data CToken = CTok_Identifier SourceCode
            | CTok_Symbol CSymbol
            | CTok_String SourceCode
            | CTok_Number CNumber
            | CTok_Keyword CKeyword
            | CTok_EOL
            | CTok_EOF
