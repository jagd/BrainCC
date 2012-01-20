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

{-

The arguments of a macro have higher privileg as the other macros.
e.g.:

#define M1 "M1"
#define M2(M1) M1 + 2

M2(1)        => results 1 + 2, not "M1" + 2

-}

import qualified Data.Map as M
import CToken

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


-- | Tokens for the preprocessor
data CPrepToken = CPTok_INCLUDE -- ^ #include
                | CPTok_DEFINE -- ^ #define
                | CPTok_IF -- ^ #if
                | CPTok_IFDEF -- ^ #ifdef
                | CPTok_IFNDEF -- ^ #ifndef
                | CPTok_ELSE -- ^ #else
                | CPTok_ENDIF -- ^ #endif
                | CPTok_RawString SourceCode
                -- ^ string with \" \" and escape \\
                | CPTok_RawChar SourceCode
                -- ^ char with \' \' and escape \\
                | CPTok_Symbol CSymbol
                -- ^ only the preprocessor relevant symbols
                | CPTok_Other SourceCode
                -- ^ other tokens, that the preprocessor do not care
                | CPTok_EOL
                | CPTok_EOF


-- type PrepParser a = ErrorT String (StateT PrepState (WriterT String IO)) a

_azAZ = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
_azAZ09 = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
standardEOL = "\n"
