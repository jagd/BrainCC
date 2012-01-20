{- |
        It is saddly, that Parsec version 2 do not has Transformer !
        So, using Parsec 3.

        C Preprocessor

                supported patterns:

                    * #include

                    * #define M X

                    * #define M(ARGS) ARGS

                    * #undef M

                    * #if Express

                    * #ifdef M

                    * #ifndef M

                    * #else

                    * #endif

                supported comment style:

                    * \/**\/

                    and

                    * \/\/

        Line number and file name will written
        in a individual line like:

                * # LINENUMBER \"FILENAME\"

        The Arguments of the Macro Pattern

                    * #define M(ARGS) ARGS

            will be seperated by Commas,

                * except those in Quotes \'\' \"\"

                * or those in parenthesis \(\)

        The steps:

            * replace \/* ..  *\/ with white spaces

            * replace \/\/ with white spaces

            * process #define #ifdef #ifndef #else #endif and #include

            * comments in the included file will be replaced at first
-}

module PREP where

import qualified Data.Map as M

type SourceCode = String

-- | remove \/* .. *\/ comments
removeCommentBlock :: SourceCode -> SourceCode
removeCommentBlock [] = []
removeCommentBlock (x:[]) = [x]
removeCommentBlock ('/':'*':xs) = ' ' : ' ' : f xs
        where f [] = error "unterminated comment"
              f (x:[]) = error "unterminated comment"
              f ('*':'/':xs) = ' ' : ' ' : removeCommentBlock xs
              f (x:xs) | isSpace' x = x : f xs
                       | otherwise = ' ' : f xs
removeCommentBlock (x:xs) = x : removeCommentBlock xs


-- | remove \/\/ comments
removeCommentLine :: SourceCode -> SourceCode
removeCommentLine [] = []
removeCommentLine (x:[]) = [x]
removeCommentLine ('/':'/':xs) = ' ' : ' ' : f xs
        where f (x:xs) | x == '\n' || x == '\r' = x : removeCommentLine xs
                       | otherwise = ' ' : f xs
removeCommentLine (x:xs) = x : removeCommentLine xs

-- | remove C89 \/* .. *\/ and C99 \/\/ Comments
removeComments :: SourceCode -> SourceCode
removeComments = removeCommentLine . removeCommentBlock

-- | the isSpace function in "Prelude" has some problem with unicode
isSpace' :: Char -> Bool
isSpace' '\n' = True
isSpace' '\r' = True
isSpace' '\t' = True
isSpace' ' ' = True
isSpace' _ = False


data CSymbol = S_LP -- ^ (
             | S_RP -- ^ )
             | S_COMMA -- ^ ,
             | S_LC -- ^ {
             | S_RC -- ^ }
             | S_LB -- ^ [
             | S_RB -- ^ ]
             | S_DOT -- ^ .
             | S_AND -- ^ &
             | S_STAR -- ^ \*
             | S_PLUS -- ^ +
             | S_MINUS -- ^ \-
             | S_NEGATE -- ^ ~
             | S_NOT -- ^ !
             | S_DIV -- ^ \/
             | S_MOD -- ^ %
             | S_LT -- ^ <
             | S_GT -- ^ \>
             | S_XOR -- ^ ^
             | S_OR -- ^ |
             | S_QUESTION -- ^ ?
             | S_COLON -- ^ :
             | S_SEMICOLON -- ^ ;
             | S_ASSIGN -- ^ =
             | S_ARROW -- ^ \->
             | S_ICR -- ^ ++
             | S_DECR -- ^ \--
             | S_LS -- ^ <<
             | S_RS -- ^ >>
             | S_LE -- ^ <=
             | S_GE -- ^ >=
             | S_EQ -- ^ ==
             | S_NE -- ^ !=
             | S_ANDAND -- ^ &&
             | S_OROR -- ^ ||
             | S_MULT_ASSIGN -- ^ \*=
             | S_DIV_ASSIGN -- ^ \/=
             | S_MOD_ASSIGN -- ^ %=
             | S_PLUS_ASSIGN -- ^ +=
             | S_MINUS_ASSIGN -- ^ \-=
             | S_LS_ASSIGN -- ^ <<=
             | S_RS_ASSIGN -- ^ \>>=
             | S_AND_ASSIGN -- ^ &=
             | S_OR_ASSIGN -- ^ |=
             | S_XOR_ASSIGN -- ^ |=
             | S_ELLIPSIS -- ^ ...


data CToken = CTok_Identifier SourceCode
            | CTok_Symbol CSymbol
            | CTok_String SourceCode
            | CTok_Number -- FIXME
            | CTok_Keyword -- FIXME
            | CTok_EOL
            | CTok_EOF

-- | tokens for the preprocessor
data CPrepToken =
        CPTok_INCLUDE -- ^ #include
      | CPTok_DEFINE -- ^ #define
      | CPTok_IF -- ^ #if
      | CPTok_IFDEF -- ^ #ifdef
      | CPTok_IFNDEF -- ^ #ifndef
      | CPTok_ELSE -- ^ #else
      | CPTok_ENDIF -- ^ #endif
      | CPTok_RawString SourceCode -- ^ string with \"\" and escape \\
      | CPTok_RawChar SourceCode -- ^ char with \'\' and escape \\
      | CPTok_Symbol CSymbol -- ^ only the preprocessor relevant symbols
      | CPTok_Other -- ^ other tokens, that the preprocessor do not care


-- type PrepParser a = ErrorT String (StateT PrepState (WriterT String IO)) a

_azAZ = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
_azAZ09 = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
standardEOL = "\n"
